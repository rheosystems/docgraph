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
  H.body $ do
    H.div ! A.class_ "container" $ body
    H.script ! A.src "https://code.jquery.com/jquery-3.3.1.slim.min.js"
             ! H.customAttribute "integrity" "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo"
             ! H.customAttribute "crossorigin" "anonymous" $ ""
    H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.0/umd/popper.min.js"
             ! H.customAttribute "integrity" "sha384-cs/chFZiN24E4KMATLdqdvsezGxaGsi4hLGOzlXwp5UZB1LY//20VyM2taTB4QvJ"
             ! H.customAttribute "crossorigin" "anonymous" $ ""
    H.script ! A.src "https://stackpath.bootstrapcdn.com/bootstrap/4.1.0/js/bootstrap.min.js"
             ! H.customAttribute "integrity" "sha384-uefMccjFJAIv6A+rW+L4AHf99KvxDjWSu1z9VI8SKNVmz4sk7buKt/6v9KI65qnm"
             ! H.customAttribute "crossorigin" "anonymous" $ ""

formGroup :: AttributeValue -> Html -> Maybe Text -> Html
formGroup fid ftitle mval =
  H.div ! A.class_ "form-group" $ do
    H.label ! for fid $ ftitle
    input ! A.type_ "text" ! A.class_ "form-control" ! A.id fid ! A.name fid ! A.value (textValue $ fromMaybe "" mval)

listGroupItem ftitle fid =
  H.li ! A.class_ "list-group-item" $ ftitle <> toHtml fid
