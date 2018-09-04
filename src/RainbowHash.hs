{-# LANGUAGE OverloadedStrings #-}
module RainbowHash ( put
                   , putFile
                   , get
                   , getFile
                   , exists
                   , allHashes
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

type Hash = String

-- | Return the 'String' of the SHA256 hash of the given 'ByteString'.
hash :: B.ByteString -> Hash
hash bs = show $ (C.hash bs :: C.SHA256)

-- |Adds the file at the given 'FilePath' and returns the hash.
putFile :: FilePath -- ^Path to storage directory.
        -> FilePath -- ^Path to the file to be stored.
        -> IO Hash  -- ^Returns the hash of the stored data.
putFile store fp = B.readFile fp >>= put store

-- | Stores the given 'ByteString' in storage and returns the SHA256 hash of its
-- contents.
put :: FilePath     -- ^Path to storage directory.
    -> B.ByteString -- ^The raw data to store.
    -> IO Hash      -- ^Returns the hash of the stored data.
put store bs = do
  (h, filePath) <- writeDataToFile store bs
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

getMediaInfo :: FilePath -> IO MediaInfo
getMediaInfo file = do
  magic <- magicOpen [MagicMime]
  magicLoadDefault magic
  mime <- magicFile magic file
  return $ parseMediaInfo mime

-- | Write the data given by the 'ByteString' to a file under the directory
-- given by 'FilePath' returning the hash of the contents.
writeDataToFile :: FilePath -> B.ByteString -> IO (Hash, FilePath)
writeDataToFile dataDir bs = do
  let i = hash bs
      (d,f) = splitAt 2 i
      dirPath = dataDir </> d
      filePath = dirPath </> f
  D.createDirectoryIfMissing True dirPath
  B.writeFile filePath bs
  pure (i, filePath)

writeMetaDataToFile :: FilePath -> IO ()
writeMetaDataToFile fp = do
  (maybeMT, maybeCS) <- getMediaInfo fp
  let mediaType = fromMaybe "unknown" maybeMT
      charset = fromMaybe "unknown" maybeCS
      str = "content-type: " ++ mediaType ++ "\n" ++ "charset: " ++ charset ++ "\n"
      metaFile = fp ++ "_metadata.txt"
  appendFile metaFile str

hashToFilePath :: FilePath    -- ^Path to storage directory.
               -> Hash        -- ^The hash of a file.
               -> IO FilePath -- ^The absolute path to the file with the given hash. May not exist.
hashToFilePath storeDir h = do
  let (d,f) = splitAt 2 h
      filePath = storeDir </> d </> f
  pure filePath

-- | Returns 'True' if the given hash exists, 'False' otherwise.
exists :: FilePath -- ^Path to storage directory.
       -> Hash     -- ^The hash of a file.
       -> IO Bool  -- ^True, if a file with the given hash exists, False otherwise.
exists storeDir h = hashToFilePath storeDir h >>= D.doesFileExist

-- |Returns the 'FilePath' of the file with the given hash, if it exists.
getFile :: FilePath            -- ^Path to storage directory.
        -> Hash                -- ^The hash of the file to retrieve.
        -> IO (Maybe FilePath) -- ^The 'FilePath' of the file with the given hash, if it exists.
getFile store h = do
  fp <- hashToFilePath store h
  exists' <- D.doesFileExist fp
  if exists' then return $ Just fp
    else return Nothing

-- | Retrieve the data (as 'ByteString'), if any, associated with the given
-- hash.
get :: FilePath                -- ^Path to storage directory.
    -> Hash                    -- ^The hash of the file to retrieve.
    -> IO (Maybe B.ByteString) -- ^The raw data of the file with the given hash, if it exists.
get store h = do
  maybeFp <- getFile store h
  case maybeFp of
    Just fp -> fmap Just (B.readFile fp)
    Nothing -> return Nothing

allHashes :: FilePath  -- ^Path to storage directory.
          -> IO [Hash] -- ^A list of all the stored hashes.
allHashes storeDir = do
  firstTwo <- D.listDirectory storeDir
  allHashes' <- sequence $ fmap hashesForHashDir firstTwo
  pure $ join allHashes'
  where hashesForHashDir :: String -> IO [Hash]
        hashesForHashDir firstTwo = do
          allFiles <- D.listDirectory $ storeDir </> firstTwo
          let subHashes = filter (not . (isSuffixOf "metadata.txt")) allFiles
          pure $ fmap (firstTwo ++) subHashes
