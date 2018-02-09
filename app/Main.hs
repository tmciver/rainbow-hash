{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.HTTP.Types (status404)
import qualified RainbowHash as RH
import Data.Maybe (maybe)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB

main :: IO ()
main = scotty 3000 $ do
  get "/blob/:hash" $ do
    h <- param "hash"
    dataMaybe <- liftIO $ RH.get h
    let strictDataMaybe = LB.fromStrict <$> dataMaybe
    maybe nouFound' raw strictDataMaybe
    where nouFound' :: ActionM ()
          nouFound' = status status404
