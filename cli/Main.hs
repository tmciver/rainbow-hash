{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude

import qualified Data.Text.IO as T
import qualified Data.Text as T
import System.IO (hFlush)
import System.FilePath
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)
import Control.Monad (join)
import qualified System.Directory as D
import qualified Data.ByteString as BS

import RainbowHash
import RainbowHash.Env (Env(..))
import RainbowHash.App (runAppIO)
import RainbowHash.CLI (Command(..), getCommand, runCommand)
import RainbowHash.CLI.Config (getConfig)

main :: IO ()
main = do
  config <- getConfig
  getCommand >>= either putStrLn (\cmd -> runReaderT (runCommand cmd) config)
