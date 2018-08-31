{-# LANGUAGE OverloadedStrings #-}
module RainbowHash ( put
                   , get
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

rainbowHashDir :: IO FilePath
rainbowHashDir = D.getXdgDirectory D.XdgData "rainbowhash"

-- | Return the 'String' of the SHA256 hash of the given 'ByteString'.
hash :: B.ByteString -> Hash
hash bs = show $ (C.hash bs :: C.SHA256)

-- | Stores the given 'ByteString' in storage and returns the SHA256 hash of its
-- contents.
put :: B.ByteString -> IO Hash
put bs = do
  dataDir <- rainbowHashDir
  (h, filePath) <- writeDataToFile dataDir bs
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

hashToFilePath :: Hash -> IO FilePath
hashToFilePath h = do
  dataDir <- rainbowHashDir
  let (d,f) = splitAt 2 h
      filePath = dataDir </> d </> f
  pure filePath

-- | Returns 'True' if the given hash exists, 'False' otherwise.
exists :: Hash -> IO Bool
exists h = hashToFilePath h >>= D.doesFileExist

-- | Retrieve the data (as 'ByteString'), if any, associated with the given
-- hash.
get :: Hash -> IO (Maybe B.ByteString)
get h = do fp <- hashToFilePath h
           exists' <- D.doesFileExist fp
           if exists'
             then Just <$> B.readFile fp
             else pure Nothing

allHashes :: IO [Hash]
allHashes = do
  dataDir <- rainbowHashDir
  firstTwo <- D.listDirectory dataDir
  allHashes' <- sequence $ fmap hashesForHashDir firstTwo
  pure $ join allHashes'
  where hashesForHashDir :: String -> IO [Hash]
        hashesForHashDir firstTwo = do
          dataDir <- rainbowHashDir
          allFiles <- D.listDirectory $ dataDir </> firstTwo
          let subHashes = filter (not . (isSuffixOf "metadata.txt")) allFiles
          pure $ fmap (firstTwo ++) subHashes
