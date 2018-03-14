{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Monoid ((<>))
import Data.Text (Text)
import DocGraph.Pages (SubmitPage(SubmitPage))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Blaze (HTML)
import Servant.Server (serve)
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)

main :: IO ()
main = do
  putStrLn "Starting docgraph..."
  run 3000 $ serve (Proxy :: Proxy Api) docgraph


type Api = Get '[HTML] SubmitPage
      :<|> "documents"
        :> ReqBody '[FormUrlEncoded] Document
        :> Post '[HTML] Text
      :<|> "static" :> Raw

docgraph :: Server Api
docgraph = getSubmitPage
      :<|> storeDocument
      :<|> serveDirectoryWebApp "static"

storeDocument :: Document -> Handler Text
storeDocument doc  =
  return $ "Received: " <> documentTitle doc

data Document = Document
  { documentTitle  :: Text
  , documentAuthor :: Text
  }

instance FromForm Document where
  fromForm f = Document
    <$> parseUnique "title" f
    <*> parseUnique "author"  f

getSubmitPage :: Handler SubmitPage
getSubmitPage = return SubmitPage
