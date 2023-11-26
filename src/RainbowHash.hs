{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

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
  , Metadata(..)
  , FileSystemRead(..)
  , CurrentTime(..)
  ) where

import Protolude hiding (readFile)

import Data.Aeson (ToJSON(..), genericToJSON, defaultOptions, fieldLabelModifier, camelTo2, genericParseJSON, FromJSON(..))
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified System.Directory as D
import System.FilePath (splitFileName)
import qualified Crypto.Hash as C
import Data.Bool (not)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Control.Monad.Logger (MonadLogger(..), logInfoN, logWarnN)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Data.Time (UTCTime)
import qualified Data.Text as T

import RainbowHash.Env (Env(..))

type Hash = Text

newtype FileId = FileId { getHash :: Hash }
  deriving (Eq, Ord, Show)
type MediaType = Text
type FileName = Text
data Metadata = Metadata
  { mediaType :: MediaType
  , fileNames :: NESet FileName
  , uploadedAt :: UTCTime
  } deriving (Eq, Show, Generic)

-- TODO: consider moving this to a second library to keep this module abstract.
instance ToJSON Metadata where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_'}

instance FromJSON Metadata where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_'}

data File = File
  { fileId :: FileId
  , fileMetadata :: Metadata
  , fileData :: ByteString
  }

class Monad m => FileGet m where
  getFile :: FileId -> m (Maybe File)
  fileExists :: FileId -> m Bool
  allFileIds :: m (Set FileId)

-- This is a low-level class used for implementation.  If you want to put a file
-- in the store, use putFileByteString or putFileFromFilePath.
class Monad m => FilePut m v where
  putFile :: FileId -> Metadata -> v -> m ()

class MediaTypeDiscover m v where
  getMediaType :: v -> m MediaType

class FileSystemRead m where
  readFile :: FilePath -> m ByteString

class CurrentTime m where
  getCurrentTime :: m UTCTime

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
     , CurrentTime m
     , MonadLogger m
     )
  => ByteString
  -> FileName
  -> m FileId
putFileByteString bs fileName = do
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
    -- TODO: get exising metadata and add this file name to the set of file names.
    else do
      logInfoN "This content does not exist in the store; adding."

      mediaType <- getMediaType bs >>= logMediaType
      time <- getCurrentTime
      let fileNames = NES.singleton fileName
          metadata = Metadata mediaType fileNames time

      putFile fileId metadata bs

      logInfoN "Put file complete."

  pure fileId

-- Put the file at the given path in the store. The file's name will be stored
-- as metadata. If you'd like to use a different file name for the metadata (if,
-- for example, the file at the given path is different from the name on the
-- client and you want to record the client name), the use putFileFromFilePath'
-- instead.
putFileFromFilePath
  :: ( FileGet m
     , FilePut m ByteString
     , MediaTypeDiscover m FilePath
     , FileSystemRead m
     , CurrentTime m
     , MonadLogger m
     )
  => FilePath -- ^The path to the file that is accessible by the server.
  -> m FileId
putFileFromFilePath fp =
  fp & splitFileName
    <&> T.pack
     & uncurry putFileFromFilePath'

-- Put the file at the given path in the store. The given file name is used for
-- metadata rather than the name of the file at the given path.  If these file
-- names are the same, then use putFileFromFilePath instead.
putFileFromFilePath'
  :: ( FileGet m
     , FilePut m ByteString
     , MediaTypeDiscover m FilePath
     , FileSystemRead m
     , CurrentTime m
     , MonadLogger m
     )
  => FilePath -- ^The path to the file that is accessible by the server.
  -> FileName -- ^The name of the file on the client.
  -> m FileId
putFileFromFilePath' fp fileName = do
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
      time <- getCurrentTime
      let fileNames = NES.singleton fileName
          metadata = Metadata mediaType fileNames time

      -- Put the file
      putFile fileId metadata bs

      logInfoN "Put file complete."

  pure fileId

-- | Return the 'Hash' of the given 'ByteString'.
calcHash :: B.ByteString -> Hash
calcHash bs = show $ (C.hash bs :: C.SHA256)
