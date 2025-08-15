{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.Server.Config
  ( Config(..)
  ) where

import Protolude

data Config = Config
  { storageDir :: FilePath
  }
  deriving (Show)
