module DocGraph.Pages where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A



data SubmitPage = SubmitPage

instance ToMarkup SubmitPage where
  toMarkup SubmitPage = do
   H.h1 "Welcome"
   H.p "Haskell"
   H.form ! A.action "/store" ! A.method "post" $ do 
     input ! A.name "name"
     input ! A.name "lastname"
     input ! A.type_ "submit"
       
