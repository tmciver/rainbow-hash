{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.HTTP.Types (status404)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified RainbowHash as RH
import Data.Maybe (maybe)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)

main :: IO ()
main = scotty 3000 $ do
  get "/blobs" showAllHashes
  get "/blob/:hash" $ param "hash" >>= getBlob

getBlob :: String -> ActionM ()
getBlob h = do
  dataMaybe <- liftIO $ RH.get h
  let strictDataMaybe = LB.fromStrict <$> dataMaybe
  maybe notFound' raw strictDataMaybe
    where notFound' :: ActionM ()
          notFound' = status status404

showAllHashes :: ActionM ()
showAllHashes = do
  allHashes <- liftIO RH.allHashes
  html $ renderHtml $ hashesHtmlView allHashes

hashesHtmlView :: [String] -> H.Html
hashesHtmlView hashes = H.docTypeHtml $ do
  H.head $ do
    H.title "All Blobs"
  H.body $ do
    H.p "A list of all blobs:"
    H.ul $ forM_ hashes (H.li . hashToAnchor)

hashToAnchor :: String -> H.Html
hashToAnchor h = ((H.a . H.toHtml) h) H.! href (fromString ("/blob/" ++ h))
