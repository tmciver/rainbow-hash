{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash
  ( getFile
  , get
  , putFile
  , put
  , exists
  , allHashes
  , Hash
  ) where

import Protolude hiding (get, put)

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import System.FilePath
import qualified System.Directory as D
import qualified Crypto.Hash as C
import Control.Monad (join)
import Magic
import Data.Bool (not)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)

import RainbowHash.App (App(..), runWithEnv)
import RainbowHash.Env (Env(..))

type Hash = Text

newtype FileId = FileId { getHash :: Hash }

class Monad m => FileGet m where
  getFile :: FileId -> m (Maybe ByteString)
  fileExists :: FileId -> m Bool

-- This is a low-level class used for implementation.  If you want to put a file
-- in the store, use putFileByteString.
class Monad m => FilePut m v where
  putFileAtHash :: Hash -> v -> m FileId

putFileByteString
  :: ( FileGet m
     , FilePut m ByteString
     )
  => ByteString
  -> m FileId
putFileByteString bs = do
  -- Get the file's hash
  let hash = calcHash bs
      fileId = FileId hash

  -- See if the file exists already.
  exists <- fileExists fileId

  -- Put the file in the store if it doesn't already exist.
  if exists
    then pure fileId
    else putFileAtHash hash bs

-- |Return the configured storage directory in the 'App' Monad.
getStorageDir :: App FilePath
getStorageDir = App $ reader storageDir

-- | Return the 'Hash' of the given 'ByteString'.
calcHash :: B.ByteString -> Hash
calcHash bs = show $ (C.hash bs :: C.SHA256)

-- |Returns the 'FilePath' of the file with the given hash, if it exists.
getFile' :: Hash                 -- ^The hash of the file to retrieve.
        -> App (Maybe FilePath) -- ^The 'FilePath' of the file with the given hash, if it exists.
getFile' h = do
  fp <- hashToFilePath h
  exists' <- liftIO $ D.doesFileExist fp
  if exists' then return $ Just fp
    else return Nothing

-- | Retrieve the data (as 'ByteString'), if any, associated with the given
-- hash.
get :: Hash                     -- ^The hash of the file to retrieve.
    -> App (Maybe B.ByteString) -- ^The raw data of the file with the given hash, if it exists.
get h = do
  maybeFp <- getFile' h
  case maybeFp of
    Just fp ->  liftIO $ Just <$> (B.readFile fp)
    Nothing -> return Nothing

-- |Adds the file at the given 'FilePath' and returns the hash.
putFile :: FilePath -- ^Path to the file to be stored.
        -> App Hash -- ^Returns the hash of the stored data.
putFile fp = (liftIO $ B.readFile fp) >>= put

-- | Stores the given 'ByteString' in storage and returns the SHA256 hash of its
-- contents.
put :: B.ByteString -- ^The raw data to store.
    -> App Hash      -- ^Returns the hash of the stored data.
put bs = do
  (h, filePath) <- writeDataToFile bs
  writeMetaDataToFile filePath
  return h

type MediaType = Text
type Charset = Text
type MediaInfo = (Maybe MediaType, Maybe Charset)

parseMediaInfo :: Text -> MediaInfo
parseMediaInfo t = case T.splitOn ";" t of
  [m] -> (parse m, Nothing)
  m:c:[] -> (parse m, parseCharset c)
  _ -> (Nothing, Nothing)
  where parse :: Text -> Maybe Text
        parse t =
          if T.null t
          then Nothing
          else Just t
        stripCharset :: Text -> Maybe Charset
        stripCharset = T.stripPrefix "charset=" . T.strip
        parseCharset :: Text -> Maybe Charset
        parseCharset s = (parse s) >>= stripCharset

getMediaInfo :: FilePath      -- |The file for which to get 'MediaInfo'.
             -> App MediaInfo
getMediaInfo file = liftIO $ do
  magic <- magicOpen [MagicMime]
  magicLoadDefault magic
  mime <- magicFile magic file
  return $ parseMediaInfo (T.pack mime)

-- | Write the data given by the 'ByteString' to a file under the directory
-- given by 'FilePath' returning the hash of the contents.
writeDataToFile :: B.ByteString         -- ^Data to write.
                -> App (Hash, FilePath)
writeDataToFile bs = do
  storageDir <- getStorageDir
  let hash = calcHash bs
      (d,f) = splitAt 2 (T.unpack hash)
      dirPath = storageDir </> d
      filePath = dirPath </> f
  liftIO $ D.createDirectoryIfMissing True dirPath
  liftIO $ B.writeFile filePath bs
  pure (hash, filePath)

writeMetaDataToFile :: FilePath -- |The file for which to get 'MediaInfo'.
                    -> App ()
writeMetaDataToFile fp = do
  (maybeMT, maybeCS) <- getMediaInfo fp
  let mediaType = fromMaybe "unknown" maybeMT
      charset = fromMaybe "unknown" maybeCS
      str = "content-type: " <> mediaType <> "\n" <> "charset: " <> charset <> "\n"
      metaFile = fp ++ "_metadata.txt"
  liftIO $ T.appendFile metaFile str

hashToFilePath :: Hash         -- ^The hash of a file.
               -> App FilePath -- ^The absolute path to the file with the given hash. May not exist.
hashToFilePath h = do
  storeDir <- getStorageDir
  let (d,f) = T.splitAt 2 h
      filePath = storeDir </> T.unpack d </> T.unpack f
  pure filePath

-- | Returns 'True' if the given hash exists, 'False' otherwise.
exists :: Hash      -- ^The hash of a file.
       -> App Bool  -- ^True, if a file with the given hash exists, False otherwise.
exists h = hashToFilePath h >>= liftIO . D.doesFileExist

allHashes :: App [Hash] -- ^A list of all the stored hashes.
allHashes = do
  storageDir <- getStorageDir
  firstTwo <- liftIO $ D.listDirectory storageDir
  allHashes' <- liftIO $ sequence $ fmap (hashesForHashDir storageDir) firstTwo
  pure $ join allHashes'
  where hashesForHashDir :: FilePath -> FilePath -> IO [Hash]
        hashesForHashDir storageDir firstTwo = do
          allFiles <- D.listDirectory $ storageDir </> firstTwo
          let subHashes = filter (not . (isSuffixOf "metadata.txt")) allFiles
          pure $ fmap (T.pack . (firstTwo <>)) subHashes
