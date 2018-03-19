{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Monoid ((<>))
import Data.Text (Text)
import DocGraph.Pages (SubmitPage(SubmitPage), ListPage(ListPage))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Blaze (HTML)
import Servant.Server (serve)
import DocGraph.Database (Document(..), insertDocument)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)

main :: IO ()
main = do
  putStrLn "Starting docgraph..."
  run 3000 $ serve (Proxy :: Proxy Api) docgraph


type Api = Get '[HTML] SubmitPage
      :<|> "documents"
        :> Get '[HTML] ListPage
      :<|> "documents"
        :> ReqBody '[FormUrlEncoded] Document
        :> Post '[HTML] Text
      :<|> "static" :> Raw

docgraph :: Server Api
docgraph = getSubmitPage
      :<|> getListPage
      :<|> storeDocument
      :<|> serveDirectoryWebApp "static"

storeDocument :: Document -> Handler Text
storeDocument doc  = do
  mres <- liftIO $ insertDocument doc
  return $ case mres of
    Nothing -> "Something went wrong with runDB"
    Just i -> "Received: " <> fromString (show i)

getSubmitPage :: Handler SubmitPage
getSubmitPage = return SubmitPage

getListPage :: Handler ListPage
getListPage = return ListPage
