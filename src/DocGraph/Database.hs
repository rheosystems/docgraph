module DocGraph.Database where

import Data.Text (Text)
import Hasql.Connection (Connection, ConnectionError, settings, acquire)
import Hasql.Session (Session, Error, run)
import Data.Monoid ((<>))
import Control.Exception (Exception, throwIO)

runDB :: Session a -> IO a
runDB sess = do
  let s = settings "localhost" 5432 "docgraph" "docgraph" "docgraph"
  econn <- acquire s
  case (econn :: Either ConnectionError Connection) of
    Left err -> throwIO $ HasqlConnectionError err
    Right conn -> do
      eres <- run sess conn
      case eres of
        Left err -> throwIO $ HasqlError err
        Right res -> return res

data DocGraphException
  = HasqlConnectionError ConnectionError
  | HasqlError Error
  deriving Show

instance Exception DocGraphException
