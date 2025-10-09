{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module RainbowHash
  ( putFile
  , putFileFromFilePath
  , calcHash
  , Hash
  , File(..)
  , FileMetadataOnly(..)
  , FileId(..)
  , FileGet(..)
  , FilePut(..)
  , MediaTypeDiscover(..)
  , MediaTypeName
  , CharSet
  , MediaType(..)
  , Metadata(..)
  , FileSystemRead(..)
  , CurrentTime(..)
  , ToByteString(..)
  , Filter(..)
  , mediaTypeToText
  , parseMediaType
  ) where

import Protolude hiding (readFile)

import qualified Codec.Compression.GZip as GZ
import Data.Set.Ordered (OSet)
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LBS
import System.FilePath (takeFileName)
import qualified Crypto.Hash as C
import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NES
import Data.Time (UTCTime)
import qualified Data.Text as T

type Hash = Text
newtype FileId = FileId { getHash :: Hash }
  deriving (Eq, Ord, Show)
type FileName = Text
type MediaTypeName = Text
type CharSet = Text
data MediaType = MediaType
  { mediaTypeName :: MediaTypeName
  , mediaTypeCharSet :: CharSet
  } deriving (Eq, Ord, Show, Generic)

parseMediaType :: Text -> Maybe MediaType
parseMediaType t = case T.splitOn ";" t of
  [ct, charSet] -> if " charset=" `T.isPrefixOf` charSet
    then Just $ MediaType (T.strip ct) (T.drop 9 charSet)
    else Nothing
  _ -> Nothing

mediaTypeToText :: MediaType -> Text
mediaTypeToText (MediaType name' charSet) = name' <> "; charset=" <> charSet

data Metadata = Metadata
  { mediaType :: MediaType
  , fileNames :: NESet FileName
  , uploadedAt :: UTCTime
  , isCompressed :: Bool
  } deriving (Eq, Ord, Show, Generic)

data File = File
  { fileId :: FileId
  , fileMetadata :: Metadata
  , fileData :: ByteString
  }

data FileMetadataOnly = FileMetadataOnly
  { fmoId :: FileId
  , fmoMetadata :: Metadata
  } deriving (Eq, Ord)

newtype Filter = FilterByContentType MediaTypeName
  deriving (Show)

class Monad m => FileGet m where
  getFile :: FileId -> m (Maybe File)
  fileExists :: FileId -> m Bool
  fileIds :: m (Set FileId) -- all file IDs
  filesMetadata :: Maybe Filter -> m (OSet FileMetadataOnly) -- metadata for (a sub-set of) all files
  contentTypes :: m (Set MediaType)

-- This is a low-level class used for implementation.  If you want to put a file
-- in the store, use putFile.
class Monad m => FilePut m where
  putFileInternal :: FileId -> Metadata -> ByteString -> m ()

class MediaTypeDiscover m v where
  getMediaType :: v -> m MediaType

class FileSystemRead m where
  readFile :: FilePath -> m ByteString

class CurrentTime m where
  getCurrentTime :: m UTCTime

class ToByteString m v where
  toByteString :: v -> m ByteString

logMediaType
  :: MonadLogger m
  => MediaType
  -> m MediaType
logMediaType mediaType' = do
  logInfoN $ "This content has content type \"" <> mediaTypeName mediaType' <> "\""
  pure mediaType'

putFile
  :: ( FileGet m
     , FilePut m
     , MediaTypeDiscover m v
     , CurrentTime m
     , MonadLogger m
     , ToByteString m v
     )
  => v
  -> FileName
  -> m FileId
putFile v fileName = do

  logInfoN "Adding content to store."

  bs <- toByteString v

  -- Get the file's hash
  let hash' = calcHash bs
      fileId' = FileId hash'

  logInfoN $ "This content has hash identifier " <> hash'

  -- See if the file exists already.
  exists <- fileExists fileId'

  -- Put the file in the store if it doesn't already exist.
  if exists
    then logInfoN "This content already exists in the store; not adding."
    -- TODO: get exising metadata and add this file name to the set of file names.
    else do
      logInfoN "This content does not exist in the store; adding."

      mediaType' <- getMediaType v >>= logMediaType
      time <- getCurrentTime
      let fileNames' = NES.singleton fileName
          (bs', isCompressed) = if shouldCompress mediaType'
            then let compressed = LBS.toStrict . GZ.compress . LBS.fromStrict $ bs
                 in (compressed, True)
            else (bs, False)
          metadata = Metadata mediaType' fileNames' time isCompressed

      putFileInternal fileId' metadata bs'

      logInfoN "Put file complete."

  pure fileId'

shouldCompress
  :: MediaType
  -> Bool
shouldCompress MediaType {..} =
  let blacklist :: Set MediaTypeName
      blacklist = Set.fromList [ "image/png"
                               , "image/apng"
                               , "image/jpeg"
                               , "video/jpeg"
                               , "application/epub+zip"
                               , "application/gzip"
                               , "application/x-bzip"
                               , "application/x-bzip2"
                               , "application/gzip"
                               , "application/x-gzip"
                               , "application/zip"
                               , "application/x-zip"
                               , "application/x-7z-compressed"
                               , "audio/mpeg"
                               , "video/mp4"
                               , "video/mpeg"
                               ]
  in Set.notMember mediaTypeName blacklist

-- Put the file at the given path in the store. The file's name will be stored
-- as metadata. If you'd like to use a different file name for the metadata (if,
-- for example, the file at the given path is different from the name on the
-- client and you want to record the client name), the use putFile instead.
putFileFromFilePath
  :: ( FileGet m
     , FilePut m
     , MediaTypeDiscover m FilePath
     , ToByteString m FilePath
     , FileSystemRead m
     , CurrentTime m
     , MonadLogger m
     )
  => FilePath -- ^The path to the file that is accessible by the server.
  -> m FileId
putFileFromFilePath fp = do
  let fileName = T.pack $ takeFileName fp
  putFile fp fileName

-- | Return the 'Hash' of the given 'ByteString'.
calcHash :: ByteString -> Hash
calcHash bs = show (C.hash bs :: C.SHA256)
