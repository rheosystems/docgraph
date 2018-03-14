{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Aeson
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server (serve)
import Servant.HTML.Blaze
import DocGraph.Pages
import Web.FormUrlEncoded(FromForm, fromForm, parseUnique)
import Data.Monoid

main :: IO ()
main = do
  putStrLn "Starting docgraph..."
  run 3000 $ serve (Proxy :: Proxy Api) docgraph


type Api = Get '[HTML] SubmitPage
      :<|> "store" 
      :> ReqBody '[FormUrlEncoded] Person :> Post '[HTML] Text 


docgraph :: Server Api
docgraph = getDocument :<|> storeDocument



getDocument :: Handler SubmitPage
getDocument =
  return $ SubmitPage

storeDocument :: Person -> Handler Text
storeDocument p = return $ "Done : " <> lastname p

data Person = Person
  { name   :: Text
  , lastname :: Text
  }

instance FromForm Person where
  fromForm p = Person
    <$> parseUnique "name" p
    <*> parseUnique "lastname" p

  
 
