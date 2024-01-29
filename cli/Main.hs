{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import RainbowHash.Command ( getCommand, runCommand)
import RainbowHash.CLI.Config (getConfig)
import RainbowHash.App (runApp)

main :: IO ()
main = do
  config <- getConfig
  getCommand >>= either putStrLn (\cmd -> void $ runApp (runCommand cmd) config)
