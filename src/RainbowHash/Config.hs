{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.Config
  ( Config(..)
  ) where

import Protolude

data Config = Config
  { storageDir :: FilePath
  }
  deriving (Show)
