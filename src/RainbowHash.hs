module RainbowHash ( put
                   , get
                   , exists
                   ) where

import qualified Data.ByteString as B
import System.FilePath
import qualified System.Directory as D
import qualified Crypto.Hash as C

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

-- | Retrieve the data (as 'ByteString'), if any, associated with the given
-- hash.
get :: Hash -> IO (Maybe B.ByteString)
get h = do fp <- hashToFilePath h
           exists' <- D.doesFileExist fp
           if exists'
             then Just <$> B.readFile fp
             else pure Nothing
