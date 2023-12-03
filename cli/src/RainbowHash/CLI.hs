{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.CLI
  ( Command(..)
  , getCommand
  , runCommand
  ) where

import Protolude

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

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

runCommand
  :: Command
  -> IO ()
runCommand (WatchDir dir) = watchDirectory dir

uploadAction :: Action
uploadAction (Added fp _ False) = putStrLn $ "Uploading file at " <> T.pack fp
uploadAction (Added _ _ True) = putStrLn ("Directory added. Ignoring" :: Text)
uploadAction e = putStrLn $ ("Ignoring event: " :: Text) <> show e

watchDirectory :: FilePath -> IO ()
watchDirectory fp = do
  withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchDir
      mgr          -- manager
      fp           -- directory to watch
      isFileAdded  -- predicate
      uploadAction -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

    where isFileAdded :: Event -> Bool
          isFileAdded Added{} = True
          isFileAdded _ = False
