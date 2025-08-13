{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.Config
  ( Config(..)
  , getConfig
  , showConfig
  ) where

import Protolude
import Numeric.Natural (Natural)
import qualified Data.Text as T

import RainbowHash.StorageDirectory (getStorageDir)

data Config = Config
  { storageDir :: FilePath
  , port :: Natural
  }
  deriving (Show)

getConfig :: IO Config
getConfig = Config <$> getStorageDir <*> pure 3000

showConfig :: Config -> IO ()
showConfig (Config storageDir' port') = do
  putStrLn $ ("Running on port " :: Text) <> show port'
  putStrLn $ ("Using directory " :: Text) <> T.pack storageDir' <> " for blob storage."
