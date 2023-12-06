module RainbowHash.Env
  ( Env(..)
  , getEnv
  ) where

import System.Directory (getXdgDirectory, XdgDirectory (XdgData), createDirectoryIfMissing)

newtype Env = Env { storageDir :: FilePath }

getStorageDirectory :: IO FilePath
getStorageDirectory = do
  d <- getXdgDirectory XdgData "rainbowhash"
  createDirectoryIfMissing True d
  pure d

getEnv :: IO Env
getEnv = Env <$> getStorageDirectory
