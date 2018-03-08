{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Aeson
import Data.Text (Text)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server (serve)
import Data.Monoid ((<>))

main :: IO ()
main = run 3000 $ serve (Proxy :: Proxy Api) getDocument

getDocument :: Server Api
getDocument =
  return $ Document "A note on Haskell" "Mikkel Christiansen"

type Api = Get '[JSON] Document

data Document = Document
  { documentName   :: Text
  , documentAuthor :: Text
  }

instance ToJSON Document where
  toJSON (Document name author) =
    object [ "name"   .= name, "author" .= author ]
