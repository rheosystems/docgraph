
module DocGraph.Database where

import Data.Text (Text)
import Hasql.Connection
import Hasql.Session (Session, run)
import Data.Monoid ((<>))

runDB :: Session a -> IO (Maybe a)
runDB sess = do
  let s = settings "localhost" 5432 "docgraph" "docgraph" "docgraph"
  econn <- acquire s
  case (econn :: Either ConnectionError Connection) of
    Left err -> do
      print err
      return Nothing
    Right conn -> do
      eres <- run sess conn
      case eres of
        Left err -> do
          print err
          return Nothing
        Right res -> return $ Just res
