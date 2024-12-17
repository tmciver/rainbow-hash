{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Protolude hiding (get, put, catch)

import Control.Monad.Catch (catch)
import Data.Set.Ordered (OSet)
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
import RainbowHash (FileId(..), FileGet(..), File(..), Metadata(Metadata), putFile, FileMetadataOnly(..), Filter(..), MediaType(..), MediaTypeName, mediaTypeToText)
import RainbowHash.App (runAppIO)
import RainbowHash.Env (Env(..))
import RainbowHash.Server.StorageDirectory (getStorageDir)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import Network.HTTP.Types.Method (StdMethod(HEAD))

main :: IO ()
main = do
  env@(Env dir') <- getEnv
  createDirectoryIfMissing True dir'
  showEnv env
  scotty 3000 $ do
    get "/" homeView
    get "/blobs" (showHashes env)
    get "/blob/:hash" $ captureParam "hash" >>= getBlob env
    get "/content-types" (showContentTypes env)
    addroute HEAD "/blob/:hash" $ captureParam "hash" >>= headBlob env
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
  contentTypesLink
  contentListLink
  fileUploadForm

homeLink :: H.Html
homeLink = (H.a . H.toHtml) ("Home" :: Text) H.! href "/"

contentTypesLink :: H.Html
contentTypesLink =
  (H.a . H.toHtml) ("Files by content type" :: Text) H.! href "/content-types" >> H.br

contentListLink :: H.Html
contentListLink =
  (H.a . H.toHtml) ("All files" :: Text) H.! href "/blobs" >> H.br

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
      setHeader "Content-Type" (TL.fromStrict . mediaTypeToText $ mediaType)
      raw strictData
    where notFound' :: ActionM ()
          notFound' = status status404

headBlob :: Env -> Text -> ActionM ()
headBlob env hash' = do
  let fileId' = FileId hash'
  fileExists' <- liftIO $ runAppIO (RH.fileExists fileId') env
  unless fileExists' notFound'
  where notFound' :: ActionM ()
        notFound' = status status404

showHashes :: Env -> ActionM ()
showHashes env = do
  ct <- captureParam "content-type" `catch` (\(_ :: SomeException) -> pure "")
  let maybeFilter = case ct of
        "" -> Nothing
        _ -> Just $ FilterByContentType ct
  metas <- liftIO $ runAppIO (filesMetadata maybeFilter) env
  html $ renderHtml $ metasHtmlView metas

showContentTypes :: Env -> ActionM ()
showContentTypes env = do
  cts <- liftIO $ runAppIO contentTypes env
  html $ renderHtml $ contentTypesHtmlView cts

metasHtmlView :: OSet FileMetadataOnly -> H.Html
metasHtmlView metas = template "Content" $ do
  H.p "Files:"
  H.ul $ forM_ metas (H.li . fileIdToAnchor . fmoId)

contentTypesHtmlView :: Set MediaType -> H.Html
contentTypesHtmlView mts = template "Content Types" $ do
  H.p "A list of all content types:"
  H.ul $ forM_ mts (H.li . contentTypeToAnchor)

fileIdToUrl :: FileId -> TL.Text
fileIdToUrl = TL.fromStrict . ("/blob/" <>) . getHash

mediaTypeToUrl :: MediaTypeName -> TL.Text
mediaTypeToUrl = TL.fromStrict . ("/blobs?content-type=" <>)

fileIdToAnchor :: FileId -> H.Html
fileIdToAnchor fileId' = (H.a . H.toHtml) (getHash fileId') H.! href (H.textValue $ toStrict $ fileIdToUrl fileId')

contentTypeToAnchor :: MediaType -> H.Html
contentTypeToAnchor (MediaType mt _) = (H.a . H.toHtml) mt H.! href (H.textValue $ toStrict $ mediaTypeToUrl mt)
