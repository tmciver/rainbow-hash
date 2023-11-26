{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module RainbowHash.App
  ( App(..)
  , runAppIO
  ) where

import Protolude

import qualified Data.Time as DT
import qualified Data.Set as Set
import qualified System.Directory as D
import qualified Data.ByteString as BS
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO)
import Magic
import Control.Monad (join)
import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr, logErrorN)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.FilePath (FilePath, (</>), takeDirectory)
import System.IO (hFlush)
import System.IO.Temp (withSystemTempFile)
import Control.Monad.Catch (MonadMask, MonadCatch, MonadThrow)
import qualified Data.Yaml as YAML

import RainbowHash.Env (Env(..))
import RainbowHash (FileGet(..), FilePut(..), MediaTypeDiscover(..), FileId(..), MediaType, File(..), FileSystemRead(..), Metadata(..), CurrentTime(..))

newtype App a = App { runApp :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadMask, MonadCatch, MonadThrow)

runAppIO :: App a -> Env -> IO a
runAppIO = runReaderT . runApp

fileIdToFilePath
  :: FileId       -- ^The ID of a file.
  -> App FilePath -- ^The absolute path to the file with the given hash. May not exist.
fileIdToFilePath (FileId hash) = do
  storeDir <- asks storageDir
  let (d,f) = T.splitAt 2 hash
      filePath = storeDir </> T.unpack d </> T.unpack f
  pure filePath

defaultMediaType :: MediaType
defaultMediaType = "application/octet-stream"

metadataFileSuffix :: Text
metadataFileSuffix = "_metadata.yaml"

getMediaTypeForFile
  :: FileId
  -> App (Maybe MediaType)
getMediaTypeForFile fileId = do
  metadataFilePath <- fileIdToFilePath fileId <&> (<> T.unpack metadataFileSuffix)
  liftIO $ Just . parseMediaTypeFromMetadata <$> T.readFile metadataFilePath

metadataContentTypePrefix :: Text
metadataContentTypePrefix = "content-type: "

parseMediaTypeFromMetadata :: Text -> MediaType
parseMediaTypeFromMetadata t = fromMaybe defaultMediaType (T.stripPrefix metadataContentTypePrefix t)

readFileMetadata
  :: FileId
  -> App (Maybe Metadata)
readFileMetadata fileId = do
  metadataFile <- (<> T.unpack metadataFileSuffix) <$> fileIdToFilePath fileId
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
writeFileMetadata fileId metadata = do
  metadataFile <- (<> T.unpack metadataFileSuffix) <$> fileIdToFilePath fileId
  liftIO $ YAML.encodeFile metadataFile metadata

instance FileGet App where
  getFile fileId = do
    fp <- fileIdToFilePath fileId
    exists' <- liftIO $ D.doesFileExist fp
    if exists'
      then do
        bs <- liftIO (BS.readFile fp)
        maybeMetadata <- readFileMetadata fileId
        case maybeMetadata of
          Nothing -> do
            logErrorN $ "Could not get metadata for file " <> show (getHash fileId)
            pure Nothing
          Just metadata -> pure . Just $ File fileId metadata bs
      else pure Nothing

  fileExists fileId =
    fileIdToFilePath fileId >>= liftIO . D.doesFileExist

  allFileIds = do
    storageDir <- asks storageDir
    firstTwo <- liftIO $ D.listDirectory storageDir
    fileIds <- liftIO $ sequence $ fmap (fileIdsForDir storageDir) firstTwo
    pure . Set.fromList $ join fileIds
      where fileIdsForDir :: FilePath -> FilePath -> IO [FileId]
            fileIdsForDir storageDir firstTwo = do
              allFiles <- D.listDirectory $ storageDir </> firstTwo
              let subHashes = filter (not . (isSuffixOf $ T.unpack metadataFileSuffix)) allFiles
              pure $ fmap (FileId . T.pack . (firstTwo <>)) subHashes

instance FilePut App ByteString where
  putFile fileId metadata bs = do
    dataFilePath <- fileIdToFilePath fileId
    let dir = takeDirectory dataFilePath
    liftIO $ D.createDirectoryIfMissing True dir
    liftIO $ BS.writeFile dataFilePath bs
    writeFileMetadata fileId metadata

instance MediaTypeDiscover App FilePath where
  getMediaType fp = do
    magic <- liftIO $ magicOpen [MagicMime]
    liftIO $ magicLoadDefault magic
    mime <- liftIO $ magicFile magic fp
    pure (T.pack mime)

instance MediaTypeDiscover App ByteString where
  getMediaType bs = withSystemTempFile "rainbowhash-" $ \fp h -> do
    liftIO $ BS.hPut h bs
    liftIO $ hFlush h
    magic <- liftIO $ magicOpen [MagicMime]
    liftIO $ magicLoadDefault magic
    mime <- liftIO $ magicFile magic fp
    pure (T.pack mime)

instance FileSystemRead App where
  readFile = liftIO . BS.readFile

instance CurrentTime App where
  getCurrentTime = liftIO DT.getCurrentTime

instance MonadLogger App where
  monadLoggerLog _ _ level msg =
    liftIO $ T.putStrLn $ (show level) <> " " <> (T.decodeUtf8 . fromLogStr . toLogStr) msg
