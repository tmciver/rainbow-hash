{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.HttpClient
  ( HttpWrite(..)
  , HttpClient
  , runHttpClient
  , HttpRead(..)
  ) where

import Protolude

import qualified Data.Text as T
import Network.HTTP.Client.MultipartFormData (partFile)
import Network.Wreq (defaults, manager, postWith, checkResponse, headWith)
import Network.HTTP.Client (Response(responseStatus), ManagerSettings (managerResponseTimeout), defaultManagerSettings, responseTimeoutMicro)
import Network.HTTP.Types (statusIsSuccessful)
import Text.URI (renderStr, URI, mkURI, render)
import Control.Lens (set, (.~))
import Control.Monad.Catch (MonadThrow)

import RainbowHash.CLI.Config (Config(..))
import RainbowHash (Hash)

class HttpWrite m where
  postFile :: FilePath -> m ()

class HttpRead m where
  doesFileExistInStore :: Hash -> m Bool

newtype HttpClient a = HttpClient { unHttpClient :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadThrow)

instance HttpWrite HttpClient where
  postFile fp = do
    url <- (<> "/blobs") . renderStr <$> asks serverUri
    putStrLn $ "Uploading file at " <> T.pack fp <> " to " <> T.pack url
    let part = partFile "" fp
    let opts = defaults & set checkResponse (Just $ \_ _ -> pure ()) -- I'm not sure if this is working: still get an exception if server is not up.
                        & manager .~ Left defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 3000000 }
    eitherRes <- liftIO $ try $ postWith opts url part
    case eitherRes of
      Left (SomeException e) -> putStrLn $ "Got error: " <> displayException e
      Right res ->
        if statusIsSuccessful . responseStatus $ res
          then liftIO $ putStrLn ("Success uploading file." :: Text)
          else liftIO $ putStrLn ("Error uploading file." :: Text)

hashToUrl
  :: ( MonadThrow m
     , MonadReader Config m
     )
  => Hash
  -> m URI
hashToUrl h = do
  host <- asks serverUri
  mkURI $ render host <> "/blob/" <> h

instance HttpRead HttpClient where
  doesFileExistInStore h = do
    fileUrl <- hashToUrl h
    liftIO . print . render $ fileUrl
    let opts = defaults & set checkResponse (Just $ \_ _ -> pure ()) -- I'm not sure if this is working: still get an exception if server is not up.
                        & manager .~ Left defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 3000000 }
    eitherRes <- liftIO $ try $ headWith opts (T.unpack . render $ fileUrl)
    liftIO . print $ eitherRes
    pure $ case eitherRes of
      Left (SomeException _) -> False
      Right res -> statusIsSuccessful . responseStatus $ res

runHttpClient :: HttpClient a -> Config -> IO a
runHttpClient = runReaderT . unHttpClient
