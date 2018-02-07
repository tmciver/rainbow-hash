module RainbowHash where

import qualified Data.ByteString as B
import System.IO (Handle)
import System.FilePath
import qualified System.Directory as D
import qualified Crypto.Hash as C

type Hash = String

rainbowHashDir :: IO FilePath
rainbowHashDir = D.getXdgDirectory D.XdgData "rainbowhash"

-- | Return the 'String' of the SHA256 hash of the given 'ByteString'.
hash :: B.ByteString -> Hash
hash bs = show $ (C.hash bs :: C.SHA256)

-- | Stores the contents of the file in storage and returns the SHA256 hash of
-- its contents.
put :: Handle -> IO Hash
put h = do
  dataDir <- rainbowHashDir
  bs <- B.hGetContents h
  writeDataToFile dataDir bs

-- | Write the data given by the 'ByteString' to a file under the directory
-- given by 'FilePath' returning the hash of the contents.
writeDataToFile :: FilePath -> B.ByteString -> IO Hash
writeDataToFile dataDir bs = do
  let i = hash bs
      (d,f) = splitAt 2 i
      dirPath = dataDir </> d
      filePath = dirPath </> f
  D.createDirectoryIfMissing True dirPath
  B.writeFile filePath bs
  pure i

hashToFilePath :: Hash -> IO FilePath
hashToFilePath h = do
  dataDir <- rainbowHashDir
  let (d,f) = splitAt 2 h
      filePath = dataDir </> d </> f
  pure filePath

-- | Returns 'True' if the given hash exists, 'False' otherwise.
exists :: Hash -> IO Bool
exists h = hashToFilePath h >>= D.doesFileExist

get :: Hash -> IO (Maybe B.ByteString)
get h = do fp <- hashToFilePath h
           exists' <- D.doesFileExist fp
           if exists'
             then Just <$> B.readFile fp
             else pure Nothing
