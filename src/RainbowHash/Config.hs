module RainbowHash.Config
  ( Config(..)
  ) where

newtype Config = Config { storageDir :: FilePath }
  deriving (Show)
