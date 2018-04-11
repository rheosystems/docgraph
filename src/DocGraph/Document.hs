{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph.Document where

import Data.Monoid ((<>))
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

getSubmitPage :: Handler SubmitPage
getSubmitPage = return SubmitPage

data SubmitPage = SubmitPage

instance ToMarkup SubmitPage where
  toMarkup SubmitPage =
    applyHead $ do
      H.h1 "Submit Document Info"
      H.form ! A.action "/documents" ! A.method "post" $ do
        formGroup "title" "Title"
        formGroup "author" "Author"
        formGroup "reference" "Reference"
        formGroup "version" "Version"
        formGroup "keywords" "Key words"
        formGroup "url" "URL"
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
      listGroupItem "Title: " $ documentTitle d
      listGroupItem "Author: " $ documentAuthor d
      listGroupItem "Reference: " $ documentRef d
      listGroupItem "version: " $ documentVer d
      listGroupItem "Key words: " $ documentKeyWords d
      listGroupItem "URL" $ fromMaybe "url unavailable" (documentUrl d)

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
    q = statement sql encoder decoder True

    sql = "insert into documents(title, author, reference, version, keywords, url ) values ($1, $2, $3, $4, $5, $6) returning document_id"

    encoder :: E.Params Document
    encoder = contramap documentTitle    (E.value E.text) <>
              contramap documentAuthor   (E.value E.text) <>
              contramap documentRef      (E.value E.text) <>
              contramap documentVer      (E.value E.text) <>
              contramap documentKeyWords (E.value E.text) <>
              contramap documentUrl      (E.nullableValue E.text)

    decoder :: D.Result Int64
    decoder = D.singleRow (D.value D.int8)


selectDocuments :: IO (Maybe [Document])
selectDocuments = runDB $ query () q
  where
    q :: Query () [Document]
    q = statement sql encoder decoder True

    sql = "select title, author, reference, version, keywords, url from documents"

    encoder :: E.Params ()
    encoder = E.unit

    decoder :: D.Result [Document]
    decoder = D.rowsList $ Document <$> D.value D.text
                                    <*> D.value D.text
                                    <*> D.value D.text
                                    <*> D.value D.text
                                    <*> D.value D.text
                                    <*> D.nullableValue D.text
