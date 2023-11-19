{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude hiding (get, put)

import Web.Scotty hiding (put)
import Network.HTTP.Types (status404)
import Network.Wai.Parse
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LB
import Data.String (fromString)
import qualified Data.Text.Lazy as TL
import qualified System.Directory as D
import Network.HTTP.Types.Status (status201)

import qualified RainbowHash as RH
import RainbowHash (FileId(..), Hash, FileGet(..), putFileByteString, File(..), MediaType)
import RainbowHash.App (runAppIO)
import RainbowHash.Env (Env(..))

rhEnv :: ActionM Env
rhEnv = liftIO $ Env <$> D.getXdgDirectory D.XdgData "rainbowhash"

main :: IO ()
main = do
  createLocalStorageDir
  scotty 3000 $ do
    get "/" homeView
    get "/blobs" showAllHashes
    get "/blob/:hash" $ param "hash" >>= getBlob
    post "/blobs" handleUpload

createLocalStorageDir :: IO ()
createLocalStorageDir = D.getXdgDirectory D.XdgData "rainbowhash" >>= D.createDirectoryIfMissing True

template :: Text -> H.Html -> H.Html
template title' body' = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.text $ "Rainbow Hash - " <> title'
  H.body $ homeLink >> H.br >> body'

homeView :: ActionM ()
homeView = html $ renderHtml homeHtml

homeHtml :: H.Html
homeHtml = template "Home" $ do
  contentListLink
  fileUploadForm

homeLink :: H.Html
homeLink = ((H.a . H.toHtml) ("Home" :: Text)) H.! href "/"

contentListLink :: H.Html
contentListLink = ((H.a . H.toHtml) ("See a list of all blobs." :: Text)) H.! href "/blobs"

fileUploadForm :: H.Html
fileUploadForm = H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/blobs" $ do
  H.input H.! type_ "file" H.! name "file"
  H.br
  H.input H.! type_ "submit"

handleUpload :: ActionM ()
handleUpload = do
  fs <- files
  case headMay fs of
    Nothing -> pure ()
    Just (_, fi) -> do
      let fcontent = LB.toStrict $ fileContent fi
      env <- rhEnv
      fileId <- liftIO $ runAppIO (putFileByteString fcontent) env
      status status201
      addHeader "Location" (fileIdToUrl fileId)
      homeView

getBlob :: Text -> ActionM ()
getBlob hash = do
  let fileId = FileId hash
  env <- rhEnv
  maybeFile <- liftIO $ runAppIO (RH.getFile fileId) env
  case maybeFile of
    Nothing -> notFound'
    Just (File _ mediaType bs) -> do
      let strictData = LB.fromStrict bs
      setHeader "Content-Type" (TL.fromStrict mediaType)
      raw strictData
    where notFound' :: ActionM ()
          notFound' = status status404

showAllHashes :: ActionM ()
showAllHashes = do
  env <- rhEnv
  allHashes <- liftIO $ runAppIO allFileIds env
  html $ renderHtml $ hashesHtmlView allHashes

hashesHtmlView :: Set FileId -> H.Html
hashesHtmlView fileIds = template "Content" $ do
  H.p "A list of all blobs:"
  H.ul $ forM_ fileIds (H.li . fileIdToAnchor)

fileIdToUrl :: FileId -> TL.Text
fileIdToUrl = TL.fromStrict . ("/blob/" <>) . getHash

fileIdToAnchor :: FileId -> H.Html
fileIdToAnchor fileId = ((H.a . H.toHtml) (getHash fileId)) H.! href (H.textValue $ toStrict $ fileIdToUrl fileId)
