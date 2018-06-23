{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph.Document where

import Data.Monoid ((<>))
import Data.Aeson
import Data.Text (Text)
import Servant
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_)
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique, parseMaybe)
import DocGraph.Bootstrap (formGroup, listGroupItem, applyHead)
import DocGraph.Database (runDB)
import Hasql.Query (Query, statement)
import Hasql.Session (query)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Default
import GHC.Generics

data Document = Document
  { documentRef       :: Text
  , documentTitle     :: Text
  , documentVer       :: Text
  , documentOwner     :: Text
  , documentKey       :: Maybe Text
  , documentUrl       :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON Document where
  toEncoding = genericToEncoding defaultOptions

instance FromForm Document where
  fromForm f = Document
    <$> parseUnique "reference" f
    <*> parseUnique "title"     f
    <*> parseUnique "version"   f
    <*> parseUnique "owner"     f
    <*> parseMaybe  "keywords"  f
    <*> parseMaybe  "url"       f

instance Default (D.Row Document) where
  def = Document <$> D.value D.text
                 <*> D.value D.text
                 <*> D.value D.text
                 <*> D.value D.text
                 <*> D.nullableValue D.text
                 <*> D.nullableValue D.text

instance Default (E.Params Document) where
  def = mconcat
      [ contramap documentRef    (E.value E.text)
      , contramap documentTitle  (E.value E.text)
      , contramap documentVer    (E.value E.text)
      , contramap documentOwner  (E.value E.text)
      , contramap documentKey    (E.nullableValue E.text)
      , contramap documentUrl    (E.nullableValue E.text)
      ]

getDocumentForm :: Handler DocumentForm
getDocumentForm = return $ DocumentForm Nothing

newtype DocumentForm = DocumentForm (Maybe Document)

instance ToMarkup DocumentForm where
  toMarkup (DocumentForm mdoc) =
    applyHead $ do
      H.h1 "Submit Document Info"
      let route = case mdoc of
                    Nothing -> "/documents"
                    Just doc -> textValue $ "/document/" <> documentRef doc
      H.form ! A.action route ! A.method "post" $ do
        formGroup "reference" "Reference" (documentRef <$> mdoc)
        formGroup "title" "Title"         (documentTitle <$> mdoc)
        formGroup "version" "Version"     (documentVer <$> mdoc)
        formGroup "owner" "Owner"         (documentOwner <$> mdoc)
        formGroup "keywords" "Keywords"   (documentKey =<< mdoc)
        formGroup "url" "URL"             (documentUrl =<< mdoc)
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"

getListPage :: Handler ListPage
getListPage = ListPage <$> liftIO selectDocuments

newtype ListPage = ListPage [Document]

instance ToMarkup ListPage where
  toMarkup ( ListPage dd) = applyHead $ do
     H.h1 "List of Documents"
     mapM_ getdoc dd

getdoc :: Document -> Html
getdoc d =
 H.div ! A.id "accordion" $
    H.div ! A.class_ "card" ! A.style "width: 18rem" $ do
      H.div ! A.id "headingOne" $ do
        H.h5 ! A.class_ "card-title" $ toHtml $ documentTitle d
        H.button ! A.class_ "btn btn-link" ! A.type_ "button" ! H.dataAttribute "toggle" "collapse" ! H.dataAttribute "target" ("#"<> textValue (documentRef d)) $ "My button"
      H.div ! A.class_ "collapse show" ! H.dataAttribute "parent" "#accordion" $ do
           H.div ! A.class_ "card-body" $
             H.ul ! A.class_ "list-group list-group-flush" $ do
                listGroupItem "Owner: "    $ documentOwner d
                listGroupItem "Version: "   $ documentVer d
                listGroupItem "Keywords: " $ fromMaybe "" (documentKey d)
                listGroupItem "URL: "       $ fromMaybe "url unavailable" (documentUrl d)
           H.a ! A.href ("/document/" <> textValue (documentRef d)) ! A.class_ "btn" $ "udpate"
           H.a ! A.href ("/document/" <> textValue (documentRef d) <> "/delete") ! A.class_ "btn" $ "delete"

storeDocument :: Document -> Handler Text
storeDocument doc  = do
  liftIO $ insertDocument doc
  return "Document Received"

insertDocument :: Document -> IO Int64
insertDocument doc = runDB $ query doc q
  where
    q :: Query Document Int64
    q = statement sql def (D.singleRow $ D.value D.int8) True

    sql = "insert into documents(reference, title, version, owner, keywords, url ) values ($1, $2, $3, $4, $5, $6)"

selectDocuments :: IO [Document]
selectDocuments = runDB $ query () q
  where
    q :: Query () [Document]
    q = statement sql def def True

    sql = "select reference, title, version, owner, keywords, url from documents"

getUpdateDocumentForm :: Text -> Handler DocumentForm
getUpdateDocumentForm ref = do
  mres <- liftIO $ selectDocument ref
  return $ DocumentForm mres

selectDocument :: Text -> IO (Maybe Document)
selectDocument ref = runDB $ query ref q
  where
    q :: Query Text (Maybe Document)
    q = statement sql (E.value E.text) def True

    sql = "select reference, title, version, owner, keywords, url from documents where reference = $1"

updateDocument :: Text -> Document -> Handler Text
updateDocument ref doc = do
  _ <- liftIO $ replaceDocument ref doc
  return "updated document received"

replaceDocument :: Text -> Document -> IO Int64
replaceDocument ref doc = runDB $ query doc q
  where
    q :: Query Document Int64
    q = statement sql def def True

    sql = "update documents set title = $2, owner = $4, version = $3, keywords = $5, url = $6 where reference = $1"

deleteDocument :: Text -> Handler Text
deleteDocument ref = do
  _ <- liftIO $ eraseDocument ref
  return "Document deleted"

eraseDocument :: Text -> IO Int64
eraseDocument ref = runDB $ query ref q
 where
    q :: Query Text Int64
    q = statement sql (E.value E.text) def True

    sql = "delete from documents where reference = $1"

getDeleteDocumentForm :: Text -> Handler DeleteDocumentForm
getDeleteDocumentForm ref = do
   mres <- liftIO $ selectDocument ref
   return $ DeleteDocumentForm mres

newtype DeleteDocumentForm = DeleteDocumentForm (Maybe Document)

instance ToMarkup DeleteDocumentForm where
  toMarkup (DeleteDocumentForm Nothing) = H.h1 "No document"
  toMarkup (DeleteDocumentForm (Just mdoc)) =
    applyHead $ do
      H.h1 "Delete Document"
      H.form ! A.action ("/document/" <> toValue (documentRef mdoc) <> "/delete") ! A.method "post" $ H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Delete"
