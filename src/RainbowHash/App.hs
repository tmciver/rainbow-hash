{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module RainbowHash.App
  ( App(..)
  , runAppIO
  ) where

import Protolude

import qualified Data.Set as Set
import qualified System.Directory as D
import qualified Data.ByteString as BS
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO)
import Magic
import Control.Monad (join)
import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.FilePath (FilePath, (</>))

import RainbowHash.Env (Env(..))
import RainbowHash (FileGet(..), FilePut(..), MediaInfoGet(..), MediaInfoPut(..), FileId(..), MediaInfo(..), Charset, MediaType, File(..))

newtype App a = App { runApp :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

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

parseMediaInfo :: Text -> MediaInfo
parseMediaInfo t = case T.splitOn ";" t of
  [m] -> MediaInfo (fromMaybe defaultMediaType (parse m)) Nothing
  m:c:[] -> MediaInfo (fromMaybe defaultMediaType (parse m)) (parseCharset c)
  _ -> MediaInfo defaultMediaType Nothing
  where parse :: Text -> Maybe Text
        parse t =
          if T.null t
          then Nothing
          else Just t
        stripCharset :: Text -> Maybe Charset
        stripCharset = T.stripPrefix "charset=" . T.strip
        parseCharset :: Text -> Maybe Charset
        parseCharset s = (parse s) >>= stripCharset

instance FileGet App where
  getFile fileId = do
    fp <- fileIdToFilePath fileId
    exists' <- liftIO $ D.doesFileExist fp
    if exists'
      then do
        bs <- liftIO (BS.readFile fp)
        maybeMediaInfo <- getMediaInfo fileId
        let mediaInfo = fromMaybe (MediaInfo defaultMediaType Nothing) maybeMediaInfo
        pure . Just $ File fileId mediaInfo bs
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
              let subHashes = filter (not . (isSuffixOf "metadata.txt")) allFiles
              pure $ fmap (FileId . T.pack . (firstTwo <>)) subHashes

instance FilePut App ByteString where
  putFile (FileId hash) bs = do
    storageDir <- asks storageDir
    let (d,f) = splitAt 2 (T.unpack hash)
        dirPath = storageDir </> d
        filePath = dirPath </> f
    liftIO $ D.createDirectoryIfMissing True dirPath
    liftIO $ BS.writeFile filePath bs

instance MediaInfoGet App where
  getMediaInfo fileId = do
    fp <- fileIdToFilePath fileId
    magic <- liftIO $ magicOpen [MagicMime]
    liftIO $ magicLoadDefault magic
    mime <- liftIO $ magicFile magic fp
    pure . Just $ parseMediaInfo (T.pack mime)

instance MediaInfoPut App where
  putMediaInfo fileId md = do
    fp <- fileIdToFilePath fileId
    maybeMediaInfo <- getMediaInfo fileId
    case maybeMediaInfo of
      Just (MediaInfo mediaType maybeCS) -> do
        let str = "content-type: " <> mediaType <> maybe "" (\charset -> "\n" <> "charset: " <> charset <> "\n") maybeCS
            metaFile = fp ++ "_metadata.txt"
        liftIO $ T.appendFile metaFile str
      Nothing -> pure () -- TODO: log

instance MonadLogger App where
  monadLoggerLog _ _ level msg =
    liftIO $ T.putStrLn $ (show level) <> " " <> (T.decodeUtf8 . fromLogStr . toLogStr) msg
