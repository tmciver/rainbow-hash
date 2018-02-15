{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.HTTP.Types (status404)
import Network.Wai.Parse
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified RainbowHash as RH
import Data.Maybe (maybe)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)

main :: IO ()
main = scotty 3000 $ do
  get "/" homeView
  get "/blobs" showAllHashes
  get "/blob/:hash" $ param "hash" >>= getBlob
  post "/blobs" handleUpload

template :: String -> H.Html -> H.Html
template title' body' = H.docTypeHtml $ do
  H.head $ do
    H.title $ fromString $ "Rainbow Hash - " ++ title'
  H.body $ homeLink >> H.br >> body'

homeView :: ActionM ()
homeView = html $ renderHtml homeHtml

homeHtml :: H.Html
homeHtml = template "Home" $ do
  contentListLink
  fileUploadForm

homeLink :: H.Html
homeLink = ((H.a . H.toHtml) ("Home" :: String)) H.! href "/"

contentListLink :: H.Html
contentListLink = ((H.a . H.toHtml) ("See a list of all blobs." :: String)) H.! href "/blobs"

fileUploadForm :: H.Html
fileUploadForm = H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/blobs" $ do
  H.input H.! type_ "file" H.! name "file"
  H.br
  H.input H.! type_ "submit"

handleUpload :: ActionM ()
handleUpload = do
  fs <- files
  let (_, fi) = head fs
      fname = BS.unpack $ fileName fi
      fcontent = LB.toStrict $ fileContent fi
  h <- liftIO $ RH.put $ fcontent
  html $ renderHtml $ H.toHtml ("Wrote file: " ++ fname ++ " as hash: " ++ h)

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
hashesHtmlView hashes = template "Content" $ do
  H.p "A list of all blobs:"
  H.ul $ forM_ hashes (H.li . hashToAnchor)

hashToAnchor :: String -> H.Html
hashToAnchor h = ((H.a . H.toHtml) h) H.! href (fromString ("/blob/" ++ h))
