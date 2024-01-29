{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.Command
  ( Command(..)
  , getCommand
  , runCommand
  ) where

import Protolude

import Control.Monad.Logger (MonadLogger)
import qualified Data.Set.Ordered as OSet
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import RainbowHash.CLI (FileSystemRead(..), DirectoryWatch(..), HttpRead(..), HttpWrite(..), putFile, FileSystemWrite, watchDirectoryMoveOnError)

data Command
  = WatchDir FilePath
  | UploadFile FilePath
  | UploadDir FilePath

getCommand :: IO (Either Text Command)
getCommand = do
  args <- getArgs
  case args of
    "watch":rest -> pure $ case rest of
      [dir] -> Right $ WatchDir dir
      [] -> Left "You must supply a path to a directory for the 'watch' command."
      _ -> Left "Too many arguments for the 'watch' command."
    "upload":rest -> case rest of
      [fileOrDirectory] -> do
        isDir <- doesDirectoryExist fileOrDirectory
        pure . Right $ if isDir
          then UploadDir fileOrDirectory
          else UploadFile fileOrDirectory
      [] -> pure . Left $ "You must supply a path to a file for the 'upload' command."
      _ -> pure . Left $ "Too many arguments for the 'upload' command."
    cmdStr:_ -> pure . Left $ "Unrecognized command " <> T.pack cmdStr
    [] -> pure . Left $ "You must supply a command."

runCommand
  :: ( FileSystemRead m
     , FileSystemWrite m
     , DirectoryWatch m
     , HttpRead m
     , HttpWrite m
     , MonadLogger m
     )
  => Command
  -> m ()
runCommand (WatchDir dir) = watchDirectoryMoveOnError dir
runCommand (UploadFile file) = putFile file
runCommand (UploadDir dir) = do
  files <- listDirectory dir
    <&> (fmap (dir </>) . OSet.toAscList)
    >>= filterM doesFileExist
  traverse_ putFile files
