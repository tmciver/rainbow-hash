{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module RainbowHash.App
  ( App
  , runApp
  ) where

import Protolude

import Control.Monad.Logger (MonadLogger(..), toLogStr, fromLogStr, logInfoN, logErrorN)
import qualified Data.Set.Ordered as OSet
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.MultipartFormData (partFile)
import Network.Wreq (defaults, manager, postWith, checkResponse, headWith)
import Network.HTTP.Client (Response(responseStatus), ManagerSettings (managerResponseTimeout), defaultManagerSettings, responseTimeoutMicro)
import Network.HTTP.Types (statusIsSuccessful)
import Text.URI (renderStr, URI, mkURI, render)
import Control.Lens (set, (.~))
import Control.Monad.Catch (MonadThrow)
import qualified System.Directory as Dir
import System.FSNotify (Event(..), Action, EventIsDirectory(..), withManager, watchDir)

import RainbowHash.CLI.Config (Config(..))
import RainbowHash (Hash)
import RainbowHash.CLI (HttpRead(..), HttpWrite(..), FileSystemRead (..), DirectoryWatch(..), HttpError)
import qualified Data.ByteString as BS

newtype App a = App { unApp :: ExceptT HttpError (ReaderT Config IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config, MonadThrow, MonadError HttpError)

instance HttpWrite App where
  postFile fp = do
    url <- (<> "/blobs") . renderStr <$> asks serverUri
    logInfoN $ "Uploading file at " <> T.pack fp <> " to " <> T.pack url
    let part = partFile "" fp
    let opts = defaults & set checkResponse (Just $ \_ _ -> pure ()) -- I'm not sure if this is working: still get an exception if server is not up.
                        & manager .~ Left defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 3000000 }
    eitherRes <- liftIO $ try $ postWith opts url part
    case eitherRes of
      Left (SomeException e) -> logErrorN $ "Got error: " <> T.pack (displayException e)
      Right res ->
        if statusIsSuccessful . responseStatus $ res
          then logInfoN ("Success uploading file." :: Text)
          else logErrorN ("Error uploading file." :: Text)

hashToUrl
  :: ( MonadThrow m
     , MonadReader Config m
     )
  => Hash
  -> m URI
hashToUrl h = do
  host <- asks serverUri
  mkURI $ render host <> "/blob/" <> h

instance HttpRead App where
  doesFileExistInStore h = do
    fileUrl <- hashToUrl h
    logInfoN $ render fileUrl
    let opts = defaults & set checkResponse (Just $ \_ _ -> pure ()) -- I'm not sure if this is working: still get an exception if server is not up.
                        & manager .~ Left defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 3000000 }
    eitherRes <- liftIO $ try $ headWith opts (T.unpack . render $ fileUrl)
    pure $ case eitherRes of
      Left (SomeException _) -> False
      Right res -> statusIsSuccessful . responseStatus $ res

instance FileSystemRead App where
  readFile = liftIO . BS.readFile
  listDirectory = (OSet.fromList <$>) . liftIO . Dir.listDirectory
  doesFileExist = liftIO . Dir.doesFileExist

instance DirectoryWatch App where
  watchDirectory dir action = do
    config <- ask
    liftIO $ withManager $ \mgr -> do
      -- start a watching job (in the background)
      void $ watchDir
        mgr          -- manager
        dir          -- directory to watch
        isFileAdded  -- predicate
        (uploadAction config) -- action

      -- sleep forever (until interrupted)
      forever $ threadDelay 1000000

        where isFileAdded :: Event -> Bool
              isFileAdded Added{} = True
              isFileAdded _ = False

              uploadAction :: Config -> Action
              uploadAction config (Added fp _ IsFile) = do
                e <- runApp (action fp) config
                case e of
                  Left err -> putStrLn $ ("There was an error" :: Text) <> show err
                  _ -> pure ()
              uploadAction _ (Added _ _ IsDirectory) = putStrLn ("Directory added. Ignoring" :: Text)
              uploadAction _ e = putStrLn $ ("Ignoring event: " :: Text) <> show e

instance MonadLogger App where
  monadLoggerLog _ _ _ msg = liftIO $ putStrLn (T.decodeUtf8 . fromLogStr . toLogStr $ msg)

runApp :: App a -> Config -> IO (Either HttpError a)
runApp = runReaderT . runExceptT . unApp
