module RainbowHash where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.IO (Handle)
import System.FilePath
import qualified System.Directory as D

type Hash = String

-- | Stores the contents of the file in storage and returns the SHA256 hash of
-- its contents.
put :: Handle -> IO Hash
put h = do
  dataDir <- D.getXdgDirectory D.XdgData "rainbowhash"
  bs <- B.hGetContents h
  let i = hash bs
      (d,f) = splitAt 2 i
      dirPath = dataDir </> d
      filePath = dirPath </> f
  D.createDirectoryIfMissing True dirPath
  B.writeFile filePath bs
  pure i

hash :: B.ByteString -> Hash
hash bs = C8.unpack $ B.take 32 bs
