module RainbowHash.Env
  ( Env(..)
  ) where

newtype Env = Env { storageDir :: FilePath }
  deriving (Show)
