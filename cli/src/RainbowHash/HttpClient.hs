{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module RainbowHash.HttpClient
  ( HttpWrite(..)
  , HttpClient
  , runHttpClient
  ) where

import Protolude

import qualified Data.Text as T
import Network.HTTP.Client.MultipartFormData (partFile)
import Network.Wreq (defaults, manager, postWith, checkResponse)
import Network.HTTP.Client (Response(responseStatus), ManagerSettings (managerResponseTimeout), defaultManagerSettings, responseTimeoutMicro)
import Network.HTTP.Types (statusIsSuccessful)
import RainbowHash.CLI.Config (Config(..))
import Text.URI (renderStr)
import Control.Lens (set, (.~))

class HttpWrite m where
  postFile :: FilePath -> m ()

newtype HttpClient a = HttpClient { unHttpClient :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

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

runHttpClient :: HttpClient a -> Config -> IO a
runHttpClient = runReaderT . unHttpClient
