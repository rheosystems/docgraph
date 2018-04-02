module DocGraph.Pages where

import DocGraph.Database
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_)
import Data.Monoid ((<>))
import Data.Text (Text)


data SubmitPage = SubmitPage

instance ToMarkup SubmitPage where
  toMarkup SubmitPage = do
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
        Just dd -> renderDocs dd

renderDocs :: [Document] -> Html
renderDocs ds = do
  H.ul $ forM_ ds $ \d -> do
    H.li . toHtml $ "Title: " <> documentTitle d
    H.li . toHtml $ "Author: " <> documentAuthor d
    H.li . toHtml $ "Reference: " <> documentRef d
    H.li . toHtml $ "Version: " <> documentVer d
    H.li . toHtml $ "Key word : " <> documentKeyWords d
    H.li . toHtml $ "URL: " <> case documentUrl d of
      Nothing -> "No document url"
      Just url -> url
    br

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
    input ! A.type_ "text" ! A.class_ "form-control" ! A.id fid
