{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash
  ( putFileByteString
  , putFileFromFilePath
  , Hash
  , File(..)
  , FileId(..)
  , FileGet(..)
  , FilePut(..)
  , MediaTypeDiscover(..)
  , MediaType
  , FileSystemRead(..)
  ) where

import Protolude hiding (readFile)

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified System.Directory as D
import qualified Crypto.Hash as C
import Data.Bool (not)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Control.Monad.Logger (MonadLogger(..), logInfoN, logWarnN)

import RainbowHash.Env (Env(..))

type Hash = Text

newtype FileId = FileId { getHash :: Hash }
  deriving (Eq, Ord, Show)
type MediaType = Text
data File = File
  { fileId :: FileId
  , fileMediaType :: MediaType
  , fileData :: ByteString
  }

class Monad m => FileGet m where
  getFile :: FileId -> m (Maybe File)
  fileExists :: FileId -> m Bool
  allFileIds :: m (Set FileId)

-- This is a low-level class used for implementation.  If you want to put a file
-- in the store, use putFileByteString or putFileFromFilePath.
class Monad m => FilePut m v where
  putFile :: FileId -> MediaType -> v -> m ()

class MediaTypeDiscover m v where
  getMediaType :: v -> m MediaType

class FileSystemRead m where
  readFile :: FilePath -> m ByteString

logMediaType
  :: MonadLogger m
  => MediaType
  -> m MediaType
logMediaType mediaType = do
  logInfoN $ "This content has content type \"" <> mediaType <> "\""
  pure mediaType

putFileByteString
  :: ( FileGet m
     , FilePut m ByteString
     , MediaTypeDiscover m ByteString
     , MonadLogger m
     )
  => ByteString
  -> m FileId
putFileByteString bs = do
  logInfoN "Adding content to store."

  -- Get the file's hash
  let hash = calcHash bs
      fileId = FileId hash

  logInfoN $ "This content has hash identifier " <> hash

  -- See if the file exists already.
  exists <- fileExists fileId

  -- Put the file in the store if it doesn't already exist.
  if exists
    then logInfoN "This content already exists in the store; not adding."
    else do
      logInfoN "This content does not exist in the store; adding."

      mediaType <- getMediaType bs >>= logMediaType

      putFile fileId mediaType bs

      logInfoN "Put file complete."

  pure fileId

putFileFromFilePath
  :: ( FileGet m
     , FilePut m ByteString
     , MediaTypeDiscover m FilePath
     , FileSystemRead m
     , MonadLogger m
     )
  => FilePath
  -> m FileId
putFileFromFilePath fp = do
  logInfoN "Adding content to store."

  -- Read the file's content
  bs <- readFile fp

  -- Get the file's hash
  let hash = calcHash bs
      fileId = FileId hash

  logInfoN $ "This content has hash identifier " <> hash

  -- See if the file exists already.
  exists <- fileExists fileId

  -- Put the file in the store if it doesn't already exist.
  if exists
    then logInfoN "This content already exists in the store; not adding."
    else do
      logInfoN "This content does not exist in the store; adding."

      -- Get the file's MediaType
      mediaType <- getMediaType fp >>= logMediaType

      -- Put the file
      putFile fileId mediaType bs

      logInfoN "Put file complete."

  pure fileId

-- | Return the 'Hash' of the given 'ByteString'.
calcHash :: B.ByteString -> Hash
calcHash bs = show $ (C.hash bs :: C.SHA256)
