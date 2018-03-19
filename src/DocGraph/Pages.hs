module DocGraph.Pages where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

data SubmitPage = SubmitPage

instance ToMarkup SubmitPage where
  toMarkup SubmitPage = do
    H.h1 "Submit Document Info"
    H.form ! A.action "/documents" ! A.method "post" $ do
      input ! A.type_ "text" ! A.name "title"
      input ! A.type_ "text" ! A.name "author"
      button "Submit" ! A.type_ "submit"

data ListPage = ListPage

instance ToMarkup ListPage where
  toMarkup ListPage = H.h1 "List of Documents"
