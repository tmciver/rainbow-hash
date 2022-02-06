{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RainbowHash ( getFile
                   , get
                   , putFile
                   , put
                   , exists
                   , allHashes
                   , runWithEnv
                   , Env(..)
                   , Hash
                   ) where

import qualified Data.ByteString as B
import System.FilePath
import qualified System.Directory as D
import qualified Crypto.Hash as C
import Control.Monad (join)
import Magic
import Data.Bool (not)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Text (pack, unpack, strip, stripPrefix)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

type Hash = String
data Env = Env { storageDir :: FilePath }
newtype App a = App { runApp :: ReaderT Env IO a }
              deriving (Functor, Applicative, Monad, MonadIO)

runWithEnv :: App a -> Env -> IO a
runWithEnv app = (runReaderT $ runApp app)

-- |Return the configured storage directory in the 'App' Monad.
getStorageDir :: App FilePath
getStorageDir = App $ reader storageDir

-- | Return the 'String' of the SHA256 hash of the given 'ByteString'.
hash :: B.ByteString -> Hash
hash bs = show $ (C.hash bs :: C.SHA256)

-- |Returns the 'FilePath' of the file with the given hash, if it exists.
getFile :: Hash                 -- ^The hash of the file to retrieve.
        -> App (Maybe FilePath) -- ^The 'FilePath' of the file with the given hash, if it exists.
getFile h = do
  fp <- hashToFilePath h
  exists' <- liftIO $ D.doesFileExist fp
  if exists' then return $ Just fp
    else return Nothing

-- | Retrieve the data (as 'ByteString'), if any, associated with the given
-- hash.
get :: Hash                     -- ^The hash of the file to retrieve.
    -> App (Maybe B.ByteString) -- ^The raw data of the file with the given hash, if it exists.
get h = do
  maybeFp <- getFile h
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

type MediaType = String
type Charset = String
type MediaInfo = (Maybe MediaType, Maybe Charset)

parseMediaInfo :: String -> MediaInfo
parseMediaInfo s = case splitOn ";" s of
  [m] -> (parse m, Nothing)
  m:c:[] -> (parse m, parseCharset c)
  _ -> (Nothing, Nothing)
  where parse :: String -> Maybe String
        parse [] = Nothing
        parse s = Just s
        stripCharset :: String -> Maybe Charset
        stripCharset s = do
          let ss = strip (pack s)
          cs <- stripPrefix "charset=" ss
          return $ unpack cs
        parseCharset :: String -> Maybe Charset
        parseCharset s = (parse s) >>= stripCharset

getMediaInfo :: FilePath      -- |The file for which to get 'MediaInfo'.
             -> App MediaInfo
getMediaInfo file = liftIO $ do
  magic <- magicOpen [MagicMime]
  magicLoadDefault magic
  mime <- magicFile magic file
  return $ parseMediaInfo mime

-- | Write the data given by the 'ByteString' to a file under the directory
-- given by 'FilePath' returning the hash of the contents.
writeDataToFile :: B.ByteString         -- ^Data to write.
                -> App (Hash, FilePath)
writeDataToFile bs = do
  storageDir <- getStorageDir
  let i = hash bs
      (d,f) = splitAt 2 i
      dirPath = storageDir </> d
      filePath = dirPath </> f
  liftIO $ D.createDirectoryIfMissing True dirPath
  liftIO $ B.writeFile filePath bs
  pure (i, filePath)

writeMetaDataToFile :: FilePath -- |The file for which to get 'MediaInfo'.
                    -> App ()
writeMetaDataToFile fp = do
  (maybeMT, maybeCS) <- getMediaInfo fp
  let mediaType = fromMaybe "unknown" maybeMT
      charset = fromMaybe "unknown" maybeCS
      str = "content-type: " ++ mediaType ++ "\n" ++ "charset: " ++ charset ++ "\n"
      metaFile = fp ++ "_metadata.txt"
  liftIO $ appendFile metaFile str

hashToFilePath :: Hash         -- ^The hash of a file.
               -> App FilePath -- ^The absolute path to the file with the given hash. May not exist.
hashToFilePath h = do
  storeDir <- getStorageDir
  let (d,f) = splitAt 2 h
      filePath = storeDir </> d </> f
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
  where hashesForHashDir :: FilePath -> String -> IO [Hash]
        hashesForHashDir storageDir firstTwo = do
          allFiles <- D.listDirectory $ storageDir </> firstTwo
          let subHashes = filter (not . (isSuffixOf "metadata.txt")) allFiles
          pure $ fmap (firstTwo ++) subHashes
