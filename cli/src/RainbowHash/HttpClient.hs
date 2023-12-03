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

class HttpWrite m where
  postFile :: FilePath -> m ()

newtype HttpClient a = HttpClient { unHttpClient :: ReaderT Config IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance HttpWrite HttpClient where
  postFile fp = putStrLn $ "Uploading file at " <> T.pack fp

data Config = Config

runHttpClient :: HttpClient a -> Config -> IO a
runHttpClient = runReaderT . unHttpClient
