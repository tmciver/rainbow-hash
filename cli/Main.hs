{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import RainbowHash.CLI ( getCommand, runCommand)
import RainbowHash.CLI.Config (getConfig)
import RainbowHash.HttpClient (runHttpClient)

main :: IO ()
main = do
  config <- getConfig
  getCommand >>= either putStrLn (\cmd -> void $ runHttpClient (runCommand cmd) config)
