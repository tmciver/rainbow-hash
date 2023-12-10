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
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.Status (status201)

import qualified RainbowHash as RH
import RainbowHash (FileId(..), FileGet(..), File(..), Metadata(Metadata), putFile)
import RainbowHash.App (runAppIO)
import RainbowHash.Env (Env(..))
import RainbowHash.Server.StorageDirectory (getStorageDir)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  env@(Env dir') <- getEnv
  createDirectoryIfMissing True dir'
  showEnv env
  scotty 3000 $ do
    get "/" homeView
    get "/blobs" (showAllHashes env)
    get "/blob/:hash" $ param "hash" >>= getBlob env
    post "/blobs" (handleUpload env)

getEnv :: IO Env
getEnv = Env <$> getStorageDir

showEnv :: Env -> IO ()
showEnv (Env storageDir') =
  putStrLn $ ("Using directory " :: Text) <> T.pack storageDir' <> " for blob storage."

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
homeLink = (H.a . H.toHtml) ("Home" :: Text) H.! href "/"

contentListLink :: H.Html
contentListLink = (H.a . H.toHtml) ("See a list of all blobs." :: Text) H.! href "/blobs"

fileUploadForm :: H.Html
fileUploadForm = H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/blobs" $ do
  H.input H.! type_ "file" H.! name "file"
  H.br
  H.input H.! type_ "submit"

handleUpload :: Env -> ActionM ()
handleUpload env = do
  fs <- files
  case headMay fs of
    Nothing -> pure ()
    Just (_, fi) -> do
      let fcontent = LB.toStrict $ fileContent fi
          fileName' = T.decodeUtf8 $ fileName fi
      fileId' <- liftIO $ runAppIO (putFile fcontent fileName') env
      status status201
      addHeader "Location" (fileIdToUrl fileId')
      homeView

getBlob :: Env -> Text -> ActionM ()
getBlob env hash' = do
  let fileId' = FileId hash'
  maybeFile <- liftIO $ runAppIO (RH.getFile fileId') env
  case maybeFile of
    Nothing -> notFound'
    Just (File _ (Metadata mediaType _ _) bs) -> do
      let strictData = LB.fromStrict bs
      setHeader "Content-Type" (TL.fromStrict mediaType)
      raw strictData
    where notFound' :: ActionM ()
          notFound' = status status404

showAllHashes :: Env -> ActionM ()
showAllHashes env = do
  allHashes <- liftIO $ runAppIO allFileIds env
  html $ renderHtml $ hashesHtmlView allHashes

hashesHtmlView :: Set FileId -> H.Html
hashesHtmlView fileIds = template "Content" $ do
  H.p "A list of all blobs:"
  H.ul $ forM_ fileIds (H.li . fileIdToAnchor)

fileIdToUrl :: FileId -> TL.Text
fileIdToUrl = TL.fromStrict . ("/blob/" <>) . getHash

fileIdToAnchor :: FileId -> H.Html
fileIdToAnchor fileId' = (H.a . H.toHtml) (getHash fileId') H.! href (H.textValue $ toStrict $ fileIdToUrl fileId')
