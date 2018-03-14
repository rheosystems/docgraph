module DocGraph.Pages where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A


data SubmitPage = SubmitPage 

instance ToMarkup SubmitPage where
    toMarkup SubmitPage = do
        H.h1 $ "Submit Document"
        H.form ! A.action "/store" ! A.method "Post" $ do
            H.input ! A.type_ "text" ! A.name "first" 
            H.input ! A.type_ "text" ! A.name "last"
            H.button "Submit" ! A.type_ "submit"

data LandingPage = LandingPage            
            
            
instance ToMarkup LandingPage where
    toMarkup LandingPage =
        H.docTypeHtml $ do
            H.head $ do
                H.meta ! A.charset "utf-8" 
                H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1, shrink-to-fit=no"
                H.link ! A.rel "stylesheet" ! A.href "/static/css/bootstrap.min.css"
                H.title "Hello World!"
            H.body $ do
                H.h1 "Hello World!"
                H.script "" ! A.src "https://code.jquery.com/jquery-3.2.1.slim.min.js" 
                H.script "" ! A.src "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.12.9/umd/popper.min.js"
                H.script "" ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/js/bootstrap.min.js" 



             
                