{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RainbowHash.HttpClient
  ( HttpWrite(..)
  , HttpClient
  , Config(..)
  , runHttpClient
  , getConfig
  ) where

import Protolude

import qualified Data.Text as T
import GHC.Natural (Natural)
import Text.URI (URI, mkURI, render)

class HttpWrite m where
  postFile :: FilePath -> m ()

newtype HttpClient a = HttpClient { unHttpClient :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance HttpWrite HttpClient where
  postFile fp = do
    uri <- asks serverUri
    putStrLn $ "Uploading file at " <> T.pack fp <> " to " <> render uri

newtype Config = Config
  { serverUri :: URI
  }

getConfig :: IO Config
getConfig = do
  let host = "localhost"
      port = 3000
  case mkURI ("http://" <> host <> ":" <> show port) of
    Just uri -> pure $ Config uri
    Nothing -> panic "Could not create a URI to the server."

runHttpClient :: HttpClient a -> Config -> IO a
runHttpClient = runReaderT . unHttpClient
