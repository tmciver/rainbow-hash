{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RainbowHash.App
  ( App(..)
  , runAppIO
  ) where

import Protolude hiding (readFile)

import Data.Aeson (ToJSON(..), FromJSON(..), Value(String), withText, withObject, (.:), (.=), object)
import Control.Monad (fail)
import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import qualified Data.Time as DT
import qualified Data.Set as Set
import qualified System.Directory as D
import qualified Data.ByteString as BS
import Magic
import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr, logErrorN)
import qualified Data.Text as T
import System.FilePath ((</>), takeDirectory)
import System.IO (hFlush)
import System.IO.Temp (withSystemTempFile)
import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import qualified Data.Yaml as YAML

import RainbowHash.Config (Config(..))
import RainbowHash (FileGet(..), FilePut(..), MediaTypeDiscover(..), FileId(..), File(..), FileSystemRead(..), CurrentTime(..), ToByteString(..), Filter (..), FileMetadataOnly (..))
import qualified RainbowHash as RH
import RainbowHash.Logger (writeLog)

newtype App a = App { runApp :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadMask, MonadCatch, MonadThrow)

runAppIO :: App a -> Config -> IO a
runAppIO = runReaderT . runApp

newtype MediaType = MediaType { getRHMediaType :: RH.MediaType }

instance ToJSON MediaType where
  toJSON (MediaType rhMediaType) = String $ RH.mediaTypeToText rhMediaType

instance FromJSON MediaType where
  parseJSON = withText "MediaType" $ \t ->
    maybe (fail $ "Could not parse media type" <> T.unpack t) (pure . MediaType) (RH.parseMediaType t)

newtype Metadata = Metadata { getRHMetadata :: RH.Metadata }

instance ToJSON Metadata where
  toJSON (Metadata (RH.Metadata mt fileNames uploadedAt)) = object
    [ "media_type" .= MediaType mt
    , "file_names" .= fileNames
    , "uploaded_at" .= uploadedAt
    ]

instance FromJSON Metadata where
  parseJSON = withObject "Metadata" $ \o -> do
    Metadata <$> (RH.Metadata <$> (getRHMediaType <$> (o .: "media_type"))
                              <*> o .: "file_names"
                              <*> o .: "uploaded_at")

fileIdToFilePath
  :: FileId       -- ^The ID of a file.
  -> App FilePath -- ^The absolute path to the file with the given hash. May not exist.
fileIdToFilePath (FileId h) = do
  storeDir <- asks storageDir
  let (d,f) = T.splitAt 2 h
      filePath = storeDir </> T.unpack d </> T.unpack f
  pure filePath

metadataFileSuffix :: Text
metadataFileSuffix = "_metadata.yaml"

readFileMetadata
  :: FileId
  -> App (Maybe Metadata)
readFileMetadata fileId' = do
  metadataFile <- (<> T.unpack metadataFileSuffix) <$> fileIdToFilePath fileId'
  e <- liftIO $ YAML.decodeFileEither metadataFile
  case e of
    Left err -> do
      logErrorN $ "Error attempting to parse metadata file " <> T.pack metadataFile <> ": " <> T.pack (displayException err)
      pure Nothing
    Right metadata -> pure metadata

writeFileMetadata
  :: FileId
  -> Metadata
  -> App ()
writeFileMetadata fileId' metadata = do
  metadataFile <- (<> T.unpack metadataFileSuffix) <$> fileIdToFilePath fileId'
  liftIO $ YAML.encodeFile metadataFile metadata

instance FileGet App where
  getFile fileId' = do
    fp <- fileIdToFilePath fileId'
    exists' <- liftIO $ D.doesFileExist fp
    if exists'
      then do
        bs <- liftIO (BS.readFile fp)
        maybeMetadata <- readFileMetadata fileId'
        case maybeMetadata of
          Nothing -> do
            logErrorN $ "Could not get metadata for file " <> show (getHash fileId')
            pure Nothing
          Just metadata -> pure . Just $ File fileId' (getRHMetadata metadata) bs
      else pure Nothing

  fileExists fileId' =
    fileIdToFilePath fileId' >>= liftIO . D.doesFileExist

  fileIds = do
    storageDir' <- asks storageDir
    firstTwo <- liftIO $ D.listDirectory storageDir'
    fileIds' <- liftIO $ mapM (fileIdsForDir storageDir') firstTwo
    pure . Set.fromList $ join fileIds'
      where fileIdsForDir :: FilePath -> FilePath -> IO [FileId]
            fileIdsForDir storageDir' firstTwo = do
              allFiles <- D.listDirectory $ storageDir' </> firstTwo
              let subHashes = filter (not . isSuffixOf (T.unpack metadataFileSuffix)) allFiles
              pure $ fmap (FileId . T.pack . (firstTwo <>)) subHashes

  filesMetadata maybeFilter = do
    allFileMetadata <- getAllFileMetadata
    pure $ case maybeFilter of
      Nothing -> allFileMetadata
      Just filter' -> OSet.filter (mkFilter filter') allFileMetadata

      where mkFilter :: Filter -> FileMetadataOnly -> Bool
            mkFilter (FilterByContentType desiredMediaType) (FileMetadataOnly _ (RH.Metadata (RH.MediaType mediaType' _) _ _)) =
              desiredMediaType `T.isInfixOf` mediaType'

  contentTypes = do
    allFileMetadata <- getAllFileMetadata
    allFileMetadata & OSet.toSet
                    & Set.map (RH.mediaType . fmoMetadata)
                    & foldl (flip Set.insert) Set.empty
                    & pure

getAllFileMetadata :: App (OSet FileMetadataOnly)
getAllFileMetadata = do
  fileIds' <- Set.toList <$> fileIds
  maybeMetadatas :: [Maybe Metadata] <- traverse readFileMetadata fileIds'
  let d :: [(FileId, Maybe Metadata)]
      d = zip fileIds' maybeMetadatas

      fun :: (FileId, Maybe Metadata) -> [(FileId, Metadata)] -> [(FileId, Metadata)]
      fun (_, Nothing) acc = acc
      fun (fileId', Just meta) acc = (fileId', meta) : acc

      e :: [(FileId, Metadata)]
      e = foldr fun [] d

      f :: OSet FileMetadataOnly
      f = e <&> uncurry FileMetadataOnly . fmap getRHMetadata
             & OSet.fromList

  pure f

instance FilePut App where
  putFileInternal fileId' metadata bs = do
    dataFilePath <- fileIdToFilePath fileId'
    let dir = takeDirectory dataFilePath
    liftIO $ D.createDirectoryIfMissing True dir
    liftIO $ BS.writeFile dataFilePath bs
    writeFileMetadata fileId' (Metadata metadata)

defaultMediaType :: RH.MediaType
defaultMediaType = RH.MediaType "application/octet-stream" "binary"

instance MediaTypeDiscover App FilePath where
  getMediaType fp = do
    magic <- liftIO $ magicOpen [MagicMime]
    liftIO $ magicLoadDefault magic
    mime <- liftIO $ magicFile magic fp
    let mt = mime & T.pack & RH.parseMediaType & fromMaybe defaultMediaType
    pure mt

instance MediaTypeDiscover App ByteString where
  getMediaType bs = withSystemTempFile "rainbowhash-" $ \fp h -> do
    liftIO $ BS.hPut h bs
    liftIO $ hFlush h
    magic <- liftIO $ magicOpen [MagicMime]
    liftIO $ magicLoadDefault magic
    mime <- liftIO $ magicFile magic fp
    let mt = mime & T.pack & RH.parseMediaType & fromMaybe defaultMediaType
    pure mt

instance FileSystemRead App where
  readFile = liftIO . BS.readFile

instance CurrentTime App where
  getCurrentTime = liftIO DT.getCurrentTime

instance ToByteString App ByteString where
  toByteString = pure

instance ToByteString App FilePath where
  toByteString = readFile

instance MonadLogger App where
  monadLoggerLog _ _ level =
    liftIO . writeLog level . decodeUtf8 . fromLogStr . toLogStr
