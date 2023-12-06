{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

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

import RainbowHash.CLI.Config (Config)
import RainbowHash.HttpClient (postFile, runHttpClient)

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
  :: ( MonadReader Config m
     , MonadIO m
     )
  => Command
  -> m ()
runCommand (WatchDir dir) = watchDirectory dir

watchDirectory
  :: ( MonadReader Config m
     , MonadIO m
     )
  => FilePath
  -> m ()
watchDirectory fp = do
  config <- ask
  liftIO $ withManager $ \mgr -> do
    -- start a watching job (in the background)
    watchDir
      mgr          -- manager
      fp           -- directory to watch
      isFileAdded  -- predicate
      (uploadAction config) -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

    where isFileAdded :: Event -> Bool
          isFileAdded Added{} = True
          isFileAdded _ = False

uploadAction :: Config -> Action
uploadAction config (Added fp _ False) = runHttpClient (postFile fp) config
uploadAction _ (Added _ _ True) = putStrLn ("Directory added. Ignoring" :: Text)
uploadAction _ e = putStrLn $ ("Ignoring event: " :: Text) <> show e
