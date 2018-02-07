module RainbowHash where

import qualified Data.ByteString as B
import System.IO (Handle)
import System.FilePath
import qualified System.Directory as D
import qualified Crypto.Hash as C

type Hash = String

-- | Return the 'String' of the SHA256 hash of the given 'ByteString'.
hash :: B.ByteString -> Hash
hash bs = show $ (C.hash bs :: C.SHA256)

-- | Stores the contents of the file in storage and returns the SHA256 hash of
-- its contents.
put :: Handle -> IO Hash
put h = do
  dataDir <- D.getXdgDirectory D.XdgData "rainbowhash"
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

