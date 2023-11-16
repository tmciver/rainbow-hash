{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RainbowHash.App
  ( App(..)
  , runWithEnv
  ) where

import Protolude

import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO)

import RainbowHash.Env (Env)

newtype App a = App { runApp :: ReaderT Env IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runWithEnv :: App a -> Env -> IO a
runWithEnv = runReaderT . runApp
