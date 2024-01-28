{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.CLI
  ( Command(..)
  , HttpRead(..)
  , HttpWrite(..)
  , FileSystemRead(..)
  , DirectoryWatch(..)
  , HttpError
  , getCommand
  , runCommand
  ) where

import Protolude hiding (readFile)

import Data.Set.Ordered (OSet)
import qualified Data.Set.Ordered as OSet
import Control.Monad.Logger (MonadLogger, logInfoN)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import RainbowHash (calcHash, Hash)

data Command
  = WatchDir FilePath
  | UploadFile FilePath
  | UploadDir FilePath

data HttpError = Noop
  deriving (Eq, Show)

class MonadError HttpError m => HttpWrite m where
  postFile :: FilePath -> m ()

class HttpRead m where
  doesFileExistInStore :: Hash -> m Bool

class Monad m => FileSystemRead m where
  readFile :: FilePath -> m ByteString
  listDirectory :: FilePath -> m (OSet FilePath)
  doesFileExist :: FilePath -> m Bool

class Monad m => DirectoryWatch m where
  watchDirectory
    :: FilePath -- ^Directory to watch
    -> (FilePath -> m ()) -- ^Action to run. Is passed the full path to a file that was added.
    -> m ()

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
     , DirectoryWatch m
     , HttpRead m
     , HttpWrite m
     , MonadLogger m
     )
  => Command
  -> m ()
runCommand (WatchDir dir) = watchDirectory dir putFile
runCommand (UploadFile file) = putFile file
runCommand (UploadDir dir) = do
  files <- listDirectory dir
    <&> (fmap (dir </>) . OSet.toAscList)
    >>= filterM doesFileExist
  traverse_ putFile files

putFile
  :: ( FileSystemRead m
     , HttpRead m
     , HttpWrite m
     , MonadLogger m
     )
  => FilePath
  -> m ()
putFile fp = do
  -- Calculate the hash of the file's content.
  bs <- readFile fp
  let hash' = calcHash bs

  -- Only upload the file if it doesn't exist on the server.
  fileExists <- doesFileExistInStore hash'
  if fileExists
    then logInfoN ("File exists on server; not uploading." :: Text)
    else postFile fp
