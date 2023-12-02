{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.CLI
  ( Command(..)
  , getCommand
  ) where

import Protolude

import qualified Data.Text as T

newtype Command
  = WatchDir FilePath

getCommand :: IO (Either Text Command)
getCommand = do
  args <- getArgs
  pure $ case args of
    "watch":rest -> case rest of
      [dir] -> Right $ WatchDir dir
      [] -> Left "You must supply a path to a directory for the 'watch' command."
      _ -> Left "Too many arguments for the 'watch' command."
    cmdStr:_ -> Left $ "Unrecognized command " <> T.pack cmdStr
    [] -> Left "You must supply a command."
