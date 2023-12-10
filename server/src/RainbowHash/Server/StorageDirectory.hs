module RainbowHash.Server.StorageDirectory
  ( getDefaultStorageDir
  , getStorageDirFromCLI
  , getStorageDir
  ) where

import Protolude

import System.Directory (getXdgDirectory, XdgDirectory (XdgData), createDirectoryIfMissing)

getDefaultStorageDir :: IO FilePath
getDefaultStorageDir = do
  d <- getXdgDirectory XdgData "rainbowhash"
  createDirectoryIfMissing True d
  pure d

getStorageDirFromCLI :: IO (Maybe FilePath)
getStorageDirFromCLI = do
  args <- getArgs
  pure $ case args of
    ["--storage-dir", storageDir] -> Just storageDir
    _ -> Nothing

getStorageDir :: IO FilePath
getStorageDir =
  getStorageDirFromCLI >>= maybe getDefaultStorageDir pure
