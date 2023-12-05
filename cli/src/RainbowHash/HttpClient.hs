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

import Data.Default
import qualified Data.Text as T
import GHC.Natural (Natural)
import Text.URI (URI (..), mkURI, render, renderStr, Authority (..), unRText)
import Network.HTTP.Client.MultipartFormData (partFile)
import Network.Wreq (post, defaults, manager, postWith, checkResponse)
import Network.HTTP.Client (Response(responseStatus), ManagerSettings (managerResponseTimeout), defaultManagerSettings, responseTimeoutMicro)
import Network.HTTP.Types (statusIsSuccessful)
import Control.Lens ((.~), (?~), set)
import Data.Aeson (ToJSON (..), object, (.=), FromJSON (..), withObject, (.:))
import Data.Maybe (fromJust)

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

newtype Config = Config
  { serverUri :: URI
  }

instance ToJSON Config where
  toJSON (Config uri) =
    let authority = either (panic "Could not get host from config") identity . uriAuthority $ uri
        host = unRText . authHost $ authority
        port = fromJust . authPort $ authority
    in object
       [ "server" .= object
         [ "host" .= host
         , "port" .= port
         ]
       ]

getURI :: Text -> Natural -> URI
getURI host port =
  fromJust $ mkURI ("http://" <> host <> ":" <> show port)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    server <- o .: "server"
    host <- server .: "host"
    port <- server .: "port"
    pure . Config $ getURI host port

instance Default Config where
  def = let host = "localhost"
            port = 3000
        in case mkURI ("http://" <> host <> ":" <> show port) of
             Just uri -> Config uri
             Nothing -> panic "Could not create a URI to the server."

getConfig :: IO Config
getConfig = do
  maybeConfig <- getConfigFromFile
  case maybeConfig of
    Just config -> pure config
    Nothing -> do
      writeConfig def
      pure def

writeConfig :: Config -> IO ()
writeConfig _ = pure ()

getConfigFromFile :: IO (Maybe Config)
getConfigFromFile = do
  pure Nothing

runHttpClient :: HttpClient a -> Config -> IO a
runHttpClient = runReaderT . unHttpClient
