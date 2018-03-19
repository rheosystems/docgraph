module DocGraph.Database where

import Data.Text (Text)
import Hasql.Connection
import Hasql.Session (Session, run, query)
import Hasql.Query (Query, statement)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique)
import Data.Default (def)
import Data.Functor.Contravariant (contramap)
import Data.Monoid ((<>))
import Data.Int (Int64)

runDB :: Session a -> IO (Maybe a)
runDB sess = do
  let s = settings "localhost" 5432 "docgraph" "docgraph" "docgraph"
  econn <- acquire s
  case (econn :: Either ConnectionError Connection) of
    Left err -> do
      putStrLn $ show err
      return Nothing
    Right conn -> do
      eres <- run sess conn
      case eres of
        Left err -> do
          putStrLn $ show err
          return Nothing
        Right res -> return $ Just res


insertDocument :: Document -> IO (Maybe Int64)
insertDocument doc = runDB $ query doc q
  where
    q :: Query Document Int64
    q = statement sql encoder decoder True

    sql = "insert into documents(title, author) values ($1, $2) returning document_id"

    encoder :: E.Params Document
    encoder = contramap documentTitle (E.value E.text) <>
              contramap documentAuthor (E.value E.text)

    decoder :: D.Result Int64
    decoder = D.singleRow (D.value D.int8)


selectDocuments :: IO (Maybe [Document])
selectDocuments = runDB $ query () q
  where
    q :: Query () [Document]
    q = statement sql encoder decoder True

    sql = "select title, author from documents"

    encoder :: E.Params ()
    encoder = E.unit

    decoder :: D.Result [Document]
    decoder = D.rowsList $ Document <$> D.value D.text <*> D.value D.text


data Document = Document
  { documentTitle  :: Text
  , documentAuthor :: Text
  } deriving Show

instance FromForm Document where
  fromForm f = Document
    <$> parseUnique "title" f
    <*> parseUnique "author"  f
