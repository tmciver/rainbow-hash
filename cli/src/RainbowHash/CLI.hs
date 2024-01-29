{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.CLI
  ( HttpRead(..)
  , HttpWrite(..)
  , FileSystemRead(..)
  , DirectoryWatch(..)
  , HttpError
  , putFile
  ) where

import Protolude hiding (readFile)

import Data.Set.Ordered (OSet)
import Control.Monad.Logger (MonadLogger, logInfoN)

import RainbowHash (calcHash, Hash)

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
    -> (FilePath -> m ()) -- ^Action to run. It's passed the full path to a file that was added.
    -> m ()

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
