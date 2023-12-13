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
import qualified Data.Text as T

import RainbowHash.CLI.Config (Config)
import RainbowHash.HttpClient (postFile, runHttpClient)

data Command
  = WatchDir FilePath
  | UploadFile FilePath

getCommand :: IO (Either Text Command)
getCommand = do
  args <- getArgs
  pure $ case args of
    "watch":rest -> case rest of
      [dir] -> Right $ WatchDir dir
      [] -> Left "You must supply a path to a directory for the 'watch' command."
      _ -> Left "Too many arguments for the 'watch' command."
    "upload":rest -> case rest of
      [file] -> Right $ UploadFile file
      [] -> Left "You must supply a path to a file for the 'upload' command."
      _ -> Left "Too many arguments for the 'upload' command."
    cmdStr:_ -> Left $ "Unrecognized command " <> T.pack cmdStr
    [] -> Left "You must supply a command."

runCommand
  :: ( MonadReader Config m
     , MonadIO m
     )
  => Command
  -> m ()
runCommand (WatchDir dir) = watchDirectory dir
runCommand (UploadFile file) = putFile file

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
    void $ watchDir
      mgr          -- manager
      fp           -- directory to watch
      isFileAdded  -- predicate
      (uploadAction config) -- action

    -- sleep forever (until interrupted)
    forever $ threadDelay 1000000

    where isFileAdded :: Event -> Bool
          isFileAdded Added{} = True
          isFileAdded _ = False

putFile
  :: ( MonadReader Config m
     , MonadIO m
     )
  => FilePath
  -> m ()
putFile fp =
  ask >>= liftIO . runHttpClient (postFile fp)

uploadAction :: Config -> Action
uploadAction config (Added fp _ False) = runHttpClient (postFile fp) config
uploadAction _ (Added _ _ True) = putStrLn ("Directory added. Ignoring" :: Text)
uploadAction _ e = putStrLn $ ("Ignoring event: " :: Text) <> show e
