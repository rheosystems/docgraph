module DocGraph.Bootstrap where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

applyHead :: Html -> Html
applyHead body = do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewpoint" ! A.content "width=device_width, intial-scale=1 , shrink-to-fit=no"
    H.link ! A.rel "stylesheet" ! A.href "/static/css/bootstrap.min.css "
  H.body $
    H.div ! A.class_ "container" $ body

formGroup :: AttributeValue -> Html -> Maybe Text -> Html
formGroup fid ftitle mval =
  H.div ! A.class_ "form-group" $ do
    H.label ! for fid $ ftitle
    input ! A.type_ "text" ! A.class_ "form-control" ! A.id fid ! A.name fid ! A.value (textValue $ fromMaybe "" mval)

listGroupItem ftitle fid =
  H.li ! A.class_ "list-group-item" $ ftitle <> toHtml fid
