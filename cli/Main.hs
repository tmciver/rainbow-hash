{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import RainbowHash.CLI ( getCommand, runCommand)
import RainbowHash.CLI.Config (getConfig)

main :: IO ()
main = do
  config <- getConfig
  getCommand >>= either putStrLn (\cmd -> runReaderT (runCommand cmd) config)
