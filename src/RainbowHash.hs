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
  , MediaInfoDiscover(..)
  , MediaInfo(..)
  , FileSystemRead(..)
  , Charset
  , MediaType
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
type Charset = Text
data MediaInfo = MediaInfo
  { mediaType :: MediaType
  , mediaCharset :: Maybe Charset
  }
data File = File
  { fileId :: FileId
  , fileMediaInfo :: MediaInfo
  , fileData :: ByteString
  }

class Monad m => FileGet m where
  getFile :: FileId -> m (Maybe File)
  fileExists :: FileId -> m Bool
  allFileIds :: m (Set FileId)

-- This is a low-level class used for implementation.  If you want to put a file
-- in the store, use putFileByteString or putFileFromFilePath.
class Monad m => FilePut m v where
  putFile :: FileId -> MediaInfo -> v -> m ()

class MediaInfoDiscover m v where
  getMediaInfo :: v -> m MediaInfo

class FileSystemRead m where
  readFile :: FilePath -> m ByteString

logMediaInfo
  :: MonadLogger m
  => MediaInfo
  -> m MediaInfo
logMediaInfo mediaInfo@(MediaInfo contentType maybeCharset) = do
  let charset = fromMaybe "unknown" maybeCharset
  logInfoN $ "This content has content type \"" <> contentType <> "\" and charset \"" <> charset <> "\"."
  pure mediaInfo

putFileByteString
  :: ( FileGet m
     , FilePut m ByteString
     , MediaInfoDiscover m ByteString
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

      mediaInfo <- getMediaInfo bs >>= logMediaInfo

      putFile fileId mediaInfo bs

      logInfoN "Put file complete."

  pure fileId

putFileFromFilePath
  :: ( FileGet m
     , FilePut m ByteString
     , MediaInfoDiscover m FilePath
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

      -- Get the file's MediaInfo
      mediaInfo <- getMediaInfo fp

      -- Put the file
      putFile fileId mediaInfo bs

      logInfoN "Put file complete."

  pure fileId

-- | Return the 'Hash' of the given 'ByteString'.
calcHash :: B.ByteString -> Hash
calcHash bs = show $ (C.hash bs :: C.SHA256)
