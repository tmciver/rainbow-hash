{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.CLI
  ( HttpRead(..)
  , HttpWrite(..)
  , FileSystemRead(..)
  , FileSystemWrite(..)
  , DirectoryWatch(..)
  , HttpError(..)
  , putFile
  , putFileMoveOnError
  , watchDirectoryMoveOnError
  ) where

import Protolude hiding (readFile)

import Data.Set.Ordered (OSet)
import Control.Monad.Logger (MonadLogger, logInfoN)

import RainbowHash (calcHash, Hash)
import System.FilePath ((</>))

newtype HttpError = PostError Text
  deriving (Eq, Show)

class MonadError HttpError m => HttpWrite m where
  postFile :: FilePath -> m ()

class HttpRead m where
  doesFileExistInStore :: Hash -> m Bool

class Monad m => FileSystemRead m where
  readFile :: FilePath -> m ByteString
  listDirectory :: FilePath -> m (OSet FilePath)
  doesFileExist :: FilePath -> m Bool

class Monad m => FileSystemWrite m where
  createDirectory :: FilePath -> m ()
  moveToDirectory
    :: FilePath -- ^Path of file to move
    -> FilePath -- ^Directory to move it to
    -> m ()
  deleteFile :: FilePath -> m ()

class Monad m => DirectoryWatch m where
  watchDirectory
    :: FilePath -- ^Directory to watch
    -> (FilePath -> m ()) -- ^Action to run. It gets passed the full path to a file that was added.
    -> m ()

watchDirectoryMoveOnError
  :: ( DirectoryWatch m
     , FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , MonadLogger m
     )
  => FilePath -- ^Directory to watch
  -> m ()
watchDirectoryMoveOnError dir = do
  let errorDir = dir </> "upload-errors"
  createDirectory errorDir
  watchDirectory dir (putFileMoveOnError errorDir)

putFile
  :: ( FileSystemRead m
     , FileSystemWrite m
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
    else postFile fp >> deleteFile fp

putFileMoveOnError
  :: ( FileSystemRead m
     , FileSystemWrite m
     , HttpRead m
     , HttpWrite m
     , MonadLogger m
     )
  => FilePath -- ^Directory to move file to on error
  -> FilePath -- ^Path to file to upload
  -> m ()
putFileMoveOnError errorDir fp = do
  putFile fp `catchError` (\(PostError _) -> moveToDirectory fp errorDir)
