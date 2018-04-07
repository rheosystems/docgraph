
module DocGraph.Pages where

import DocGraph.Database
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Text (Text)


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

data ListPage = ListPage (Maybe [Document])

instance ToMarkup ListPage where
  toMarkup ( ListPage dd) = applyHead $ do
     H.h1 "List of Documents"
     case dd of
        Nothing -> "nothing"
        Just dd -> mapM_ getdoc dd

applyHead :: Html -> Html
applyHead body = do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewpoint" ! A.content "width=device_width, intial-scale=1 , shrink-to-fit=no"
    H.link ! A.rel "stylesheet" ! A.href "/static/css/bootstrap.min.css "
  H.body $
    H.div ! A.class_ "container" $ body

formGroup :: AttributeValue -> Html -> Html
formGroup fid ftitle  =
  H.div ! A.class_ "form-group" $ do
    H.label ! for fid $ ftitle
    input ! A.type_ "text" ! A.class_ "form-control" ! A.id fid ! A.name fid

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
      listGroupItem "URL" $
        case (documentUrl d) of
         Nothing -> "url unvailble"
         Just url -> url

listGroupItem ftitle fid =
  H.li ! A.class_ "list-group-item" $ ftitle <> toHtml fid
