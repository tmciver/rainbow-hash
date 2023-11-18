{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash
  ( putFileByteString
  , Hash
  , FileId(..)
  , FileGet(..)
  , FilePut(..)
  , MediaInfoGet(..)
  , MediaInfoPut(..)
  , MediaInfo(..)
  , Charset
  , MediaType
  ) where

import Protolude hiding (get, put)

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
  { mediaType :: Maybe MediaType
  , mediaCharset :: Maybe Charset
  }

class Monad m => FileGet m where
  getFile :: FileId -> m (Maybe ByteString)
  fileExists :: FileId -> m Bool
  allFileIds :: m (Set FileId)

-- This is a low-level class used for implementation.  If you want to put a file
-- in the store, use putFileByteString.
class Monad m => FilePut m v where
  putFile :: FileId -> v -> m ()

class MediaInfoGet m where
  getMediaInfo :: FileId -> m (Maybe MediaInfo)

class MediaInfoPut m where
  putMediaInfo :: FileId -> MediaInfo -> m ()

putFileByteString
  :: ( FileGet m
     , FilePut m ByteString
     , MediaInfoGet m
     , MediaInfoPut m
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
      putFile fileId bs

      maybeMetadata <- getMediaInfo fileId
      case maybeMetadata of
        Just metadata -> putMediaInfo fileId metadata
        Nothing -> logWarnN $ "Could not get media info for file with ID " <> hash

  pure fileId

-- | Return the 'Hash' of the given 'ByteString'.
calcHash :: B.ByteString -> Hash
calcHash bs = show $ (C.hash bs :: C.SHA256)
