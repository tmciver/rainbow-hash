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
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.Status (status201)
import System.Directory (createDirectoryIfMissing)
import Network.HTTP.Types.Method (StdMethod(HEAD))

import qualified RainbowHash as RH
import RainbowHash (FileId(..), FileGet(..), File(..), Metadata(Metadata), putFile, FileMetadataOnly(..), Filter(..), MediaType(..), MediaTypeName, mediaTypeToText)
import RainbowHash.App (runAppIO)
import RainbowHash.Config (Config(..))
import RainbowHash.Server.StorageDirectory (getStorageDir)

main :: IO ()
main = do
  config@(Config dir') <- getConfig
  createDirectoryIfMissing True dir'
  showConfig config
  -- TODO: parameterize port
  scotty 3000 $ do
    get "/" homeView
    get "/blobs" (showHashes config)
    get "/blob/:hash" $ captureParam "hash" >>= getBlob config
    get "/content-types" (showContentTypes config)
    addroute HEAD "/blob/:hash" $ captureParam "hash" >>= headBlob config
    post "/blobs" (handleUpload config)

getConfig :: IO Config
getConfig = Config <$> getStorageDir

showConfig :: Config -> IO ()
showConfig (Config storageDir') =
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

getHost :: ActionM Text
getHost = header "Host" <&> maybe "localhost" TL.toStrict

handleUpload :: Config -> ActionM ()
handleUpload config = do
  fs <- files
  case headMay fs of
    Nothing -> pure ()
    Just (_, fi) -> do
      let fcontent = LB.toStrict $ fileContent fi
          fileName' = T.decodeUtf8 $ fileName fi
      fileId' <- liftIO $ runAppIO (putFile fcontent fileName') config
      host <- getHost
      status status201
      addHeader "Location" (fileIdToUrl host fileId')
      homeView

getBlob :: Config -> Text -> ActionM ()
getBlob config hash' = do
  let fileId' = FileId hash'
  maybeFile <- liftIO $ runAppIO (RH.getFile fileId') config
  case maybeFile of
    Nothing -> notFound'
    Just (File _ (Metadata mediaType _ _) bs) -> do
      let strictData = LB.fromStrict bs
      setHeader "Content-Type" (TL.fromStrict . mediaTypeToText $ mediaType)
      raw strictData
    where notFound' :: ActionM ()
          notFound' = status status404

headBlob :: Config -> Text -> ActionM ()
headBlob config hash' = do
  let fileId' = FileId hash'
  fileExists' <- liftIO $ runAppIO (RH.fileExists fileId') config
  unless fileExists' notFound'
  where notFound' :: ActionM ()
        notFound' = status status404

showHashes :: Config -> ActionM ()
showHashes config = do
  ct <- captureParam "content-type" `catch` (\(_ :: SomeException) -> pure "")
  let maybeFilter = case ct of
        "" -> Nothing
        _ -> Just $ FilterByContentType ct
  metas <- liftIO $ runAppIO (filesMetadata maybeFilter) config
  host <- getHost
  html $ renderHtml $ metasHtmlView host metas

showContentTypes :: Config -> ActionM ()
showContentTypes config = do
  cts <- liftIO $ runAppIO contentTypes config
  html $ renderHtml $ contentTypesHtmlView cts

metasHtmlView :: Text -> OSet FileMetadataOnly -> H.Html
metasHtmlView host metas = template "Content" $ do
  H.p "Files:"
  H.ul $ forM_ metas (H.li . fileIdToAnchor host . fmoId)

contentTypesHtmlView :: Set MediaType -> H.Html
contentTypesHtmlView mts = template "Content Types" $ do
  H.p "A list of all content types:"
  H.ul $ forM_ mts (H.li . contentTypeToAnchor)

fileIdToUrl :: Text -> FileId -> TL.Text
fileIdToUrl host = TL.fromStrict . (("http://" <> host <> "/blob/") <>) . getHash

mediaTypeToUrl :: MediaTypeName -> TL.Text
mediaTypeToUrl = TL.fromStrict . ("/blobs?content-type=" <>)

fileIdToAnchor :: Text -> FileId -> H.Html
fileIdToAnchor host fileId' = (H.a . H.toHtml) (getHash fileId') H.! href (H.textValue $ toStrict $ fileIdToUrl host fileId')

contentTypeToAnchor :: MediaType -> H.Html
contentTypeToAnchor (MediaType mt _) = (H.a . H.toHtml) mt H.! href (H.textValue $ toStrict $ mediaTypeToUrl mt)
