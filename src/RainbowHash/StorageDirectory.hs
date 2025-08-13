module RainbowHash.StorageDirectory
  ( getDefaultStorageDir
  , getStorageDirFromCLI
  , getStorageDir
  ) where

import Protolude

import System.Directory (getXdgDirectory, XdgDirectory (XdgData))

getDefaultStorageDir :: IO FilePath
getDefaultStorageDir = getXdgDirectory XdgData "rainbowhash"

getStorageDirFromCLI :: IO (Maybe FilePath)
getStorageDirFromCLI = do
  args <- getArgs
  pure $ case args of
    ["--storage-dir", storageDir] -> Just storageDir
    _ -> Nothing

getStorageDir :: IO FilePath
getStorageDir =
  getStorageDirFromCLI >>= maybe getDefaultStorageDir pure
