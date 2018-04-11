{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph.Document where

import Data.Monoid ((<>))
import Data.Text (Text)
import Servant
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_, join)
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

data Document = Document
  { documentTitle     :: Text
  , documentAuthor    :: Text
  , documentRef       :: Text
  , documentVer       :: Text
  , documentKeyWords  :: Text
  , documentUrl       :: Maybe Text
  } deriving Show

instance FromForm Document where
  fromForm f = Document
    <$> parseUnique "title"     f
    <*> parseUnique "author"    f
    <*> parseUnique "reference" f
    <*> parseUnique "version"   f
    <*> parseUnique "keywords"  f
    <*> parseMaybe  "url"       f

instance Default (D.Row Document) where
  def = Document <$> D.value D.text
                 <*> D.value D.text
                 <*> D.value D.text
                 <*> D.value D.text
                 <*> D.value D.text
                 <*> D.nullableValue D.text

instance Default (E.Params Document) where
  def = mconcat
      [ contramap documentTitle    (E.value E.text)
      ,  contramap documentAuthor  (E.value E.text)
      , contramap documentRef      (E.value E.text)
      , contramap documentVer      (E.value E.text)
      , contramap documentKeyWords (E.value E.text)
      , contramap documentUrl      (E.nullableValue E.text)
      ]

getDocumentForm :: Handler DocumentForm
getDocumentForm = return $ DocumentForm Nothing

newtype DocumentForm = DocumentForm (Maybe Document)

instance ToMarkup DocumentForm where
  toMarkup (DocumentForm mdoc) =
    applyHead $ do
      H.h1 "Submit Document Info"
      H.form ! A.action "/documents" ! A.method "post" $ do
        formGroup "title" "Title"         (documentTitle <$> mdoc)
        formGroup "author" "Author"       (documentAuthor <$> mdoc)
        formGroup "reference" "Reference" (documentRef <$> mdoc)
        formGroup "version" "Version"     (documentVer <$> mdoc)
        formGroup "keywords" "Key words"  (documentKeyWords <$> mdoc)
        formGroup "url" "URL"             (join $ documentUrl <$> mdoc)
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"

getListPage :: Handler ListPage
getListPage = ListPage <$> liftIO selectDocuments

newtype ListPage = ListPage (Maybe [Document])

instance ToMarkup ListPage where
  toMarkup ( ListPage dd) = applyHead $ do
     H.h1 "List of Documents"
     case dd of
        Nothing -> "nothing"
        Just dd -> mapM_ getdoc dd

getdoc :: Document -> Html
getdoc d =
  H.div ! A.class_ "card" ! A.style "width: 18rem" $ do
    H.div ! A.class_ "card body" $
     H.h5 ! A.class_ "doc title" $ "document info"
    H.ul ! A.class_ "list-group list-group-flush" $ do
      listGroupItem "Title: "     $ documentTitle d
      listGroupItem "Author: "    $ documentAuthor d
      listGroupItem "Reference: " $ documentRef d
      listGroupItem "version: "   $ documentVer d
      listGroupItem "Key words: " $ documentKeyWords d
      listGroupItem "URL"         $ fromMaybe "url unavailable" (documentUrl d)

storeDocument :: Document -> Handler Text
storeDocument doc  = do
  mres <- liftIO $ insertDocument doc
  return $ case mres of
    Nothing -> "Something went wrong with runDB"
    Just i -> "Received: "

insertDocument :: Document -> IO (Maybe Int64)
insertDocument doc = runDB $ query doc q
  where
    q :: Query Document Int64
    q = statement sql def (D.singleRow $ D.value D.int8) True

    sql = "insert into documents(title, author, reference, version, keywords, url ) values ($1, $2, $3, $4, $5, $6) returning document_id"

selectDocuments :: IO (Maybe [Document])
selectDocuments = runDB $ query () q
  where
    q :: Query () [Document]
    q = statement sql def def True

    sql = "select title, author, reference, version, keywords, url from documents"

getUpdateDocumentForm :: Text -> Handler DocumentForm
getUpdateDocumentForm ref = do
  mres <- liftIO $ selectDocument ref
  return $ DocumentForm mres

selectDocument :: Text -> IO (Maybe Document)
selectDocument ref = do
  mmd <- runDB $ query ref q
  return $ join mmd
  where
    q :: Query Text (Maybe Document)
    q = statement sql (E.value E.text) def True

    sql = "select title, author, reference, version, keywords, url from documents where reference = $1"
