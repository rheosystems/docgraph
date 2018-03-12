{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Aeson
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server (serve)
import Servant.HTML.Blaze
import Text.Blaze.Html

main :: IO ()
main = do
  putStrLn "Starting docgraph..."
  run 3000 $ serve (Proxy :: Proxy Api) docgraph


type Api = Get '[HTML] Document
      :<|> "store" :> Post '[JSON] Text

docgraph :: Server Api
docgraph = getDocument :<|> storeDocument

getDocument :: Handler Document
getDocument =
  return $ Document "A note on Haskell" "Mikkel Christiansen"

storeDocument :: Handler Text
storeDocument = return "Document stored..."

data Document = Document
  { documentName   :: Text
  , documentAuthor :: Text
  }

instance ToJSON Document where
  toJSON (Document name author) =
    object [ "name"   .= name, "author" .= author ]

instance ToMarkup Document where
  toMarkup (Document n _) = toHtml n
