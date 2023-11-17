{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module RainbowHash.App
  ( App(..)
  , runWithEnv
  ) where

import Protolude

import qualified Data.Set as Set
import qualified System.Directory as D
import qualified Data.ByteString as BS
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO)
import Magic
import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.FilePath (FilePath, (</>))

import RainbowHash.Env (Env(..))
import RainbowHash (FileGet(..), FilePut(..), MediaInfoGet(..), MediaInfoPut(..), FileId(..), MediaInfo(..), Charset, MediaType)

newtype App a = App { runApp :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

runWithEnv :: App a -> Env -> IO a
runWithEnv = runReaderT . runApp

fileIdToFilePath
  :: FileId         -- ^The hash of a file.
  -> App FilePath -- ^The absolute path to the file with the given hash. May not exist.
fileIdToFilePath (FileId hash) = do
  storeDir <- asks storageDir
  let (d,f) = T.splitAt 2 hash
      filePath = storeDir </> T.unpack d </> T.unpack f
  pure filePath

parseMediaInfo :: Text -> MediaInfo
parseMediaInfo t = case T.splitOn ";" t of
  [m] -> MediaInfo (parse m) Nothing
  m:c:[] -> MediaInfo (parse m) (parseCharset c)
  _ -> MediaInfo Nothing Nothing
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
    if exists' then Just <$> liftIO (BS.readFile fp)
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
      Just (MediaInfo maybeMT maybeCS) -> do
        let mediaType = fromMaybe "unknown" maybeMT
            charset = fromMaybe "unknown" maybeCS
            str = "content-type: " <> mediaType <> "\n" <> "charset: " <> charset <> "\n"
            metaFile = fp ++ "_metadata.txt"
        liftIO $ T.appendFile metaFile str
      Nothing -> pure () -- TODO: log
