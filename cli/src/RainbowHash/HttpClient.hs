{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RainbowHash.HttpClient
  ( HttpWrite(..)
  , HttpClient
  , Config(..)
  , runHttpClient
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
    uri <- getRhURL
    putStrLn $ "Uploading file at " <> T.pack fp <> " to " <> render uri

data Config = Config
  { getRhHost :: Text
  , getRhPort :: Natural
  }

getRhURL :: HttpClient URI
getRhURL = do
  Config host port <- ask
  case mkURI ("http://" <> host <> ":" <> show port) of
    Just uri -> pure uri
    Nothing -> panic "Could not create a URI to the server."

runHttpClient :: HttpClient a -> Config -> IO a
runHttpClient = runReaderT . unHttpClient
