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
  get "/blob/:hash" $ param "hash" >>= getBlob

getBlob :: String -> ActionM ()
getBlob h = do
  dataMaybe <- liftIO $ RH.get h
  let strictDataMaybe = LB.fromStrict <$> dataMaybe
  maybe notFound' raw strictDataMaybe
    where notFound' :: ActionM ()
          notFound' = status status404
