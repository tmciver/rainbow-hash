{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.CLI.Config
  ( Config(..)
  , getConfig
  ) where

import Protolude

import Data.Aeson (ToJSON (..), object, (.=), FromJSON (..), withObject, (.:))
import GHC.Natural (Natural)
import Text.URI (URI (..), mkURI, Authority (..), unRText)
import Data.Maybe (fromJust)
import qualified System.Directory as D
import System.FilePath ((</>), takeDirectory)
import qualified Data.Yaml as YAML
import Data.Default

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
            port :: Natural
            port = 3000
        in case mkURI ("http://" <> host <> ":" <> show port) of
             Just uri -> Config uri
             Nothing -> panic "Could not create a URI to the server."

getConfigFile :: IO FilePath
getConfigFile = (</> "cli" </> "config.yaml") <$> D.getXdgDirectory D.XdgConfig "rainbowhash"

getConfig :: IO Config
getConfig = do
  maybeConfig <- getConfigFromFile
  case maybeConfig of
    Just config -> pure config
    Nothing -> do
      writeConfigToFile def
      pure def

writeConfigToFile :: Config -> IO ()
writeConfigToFile config = do
  configFile <- getConfigFile
  YAML.encodeFile configFile config

getConfigFromFile :: IO (Maybe Config)
getConfigFromFile = do
  configFile <- getConfigFile
  let configDir = takeDirectory configFile
  D.createDirectoryIfMissing True configDir
  eitherConfig <- YAML.decodeFileEither configFile
  pure $ fromRight Nothing eitherConfig
