{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server (serve)
import Servant.HTML.Blaze (HTML)
import DocGraph.Pages
import Web.FormUrlEncoded



main :: IO ()
main = do
  putStrLn "Starting docgraph..."
  run 3000 $ serve (Proxy :: Proxy Api) docgraph


type Api = 
      "store"
        :> ReqBody '[FormUrlEncoded] Document
        :> Post '[JSON] Text
      :<|> "SubmitPage" :> Get '[HTML] SubmitPage
      :<|> "LandingPage" :> Get '[HTML] LandingPage

docgraph :: Server Api
docgraph =  storeDocument :<|> getPage :<|> landPage

getPage :: Handler SubmitPage
getPage = return SubmitPage
 
landPage :: Handler LandingPage
landPage = return LandingPage

storeDocument :: Document -> Handler Text
storeDocument _doc = return "Document received"

data Document = Document
  { documentName   :: Text
  , documentAuthor :: Text
  }

instance FromForm Document where
   fromForm doc = Document
       <$> parseUnique "first" doc
       <*> parseUnique "last" doc



 

