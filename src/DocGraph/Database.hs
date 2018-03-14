module DocGraph.Database (runDB, query) where

import Hasql.Connection (settings, acquire)
import Hasql.Session (Session, run, query)


runDB :: Session a -> IO (Maybe a)
runDB sess = do
  let s = settings "localhost" 5432 "docgraph" "docgraph" "docgraph"
  econn <- acquire s
  case econn of
    Left err -> do
      putStrLn $ show err
      return Nothing
    Right conn -> do
      res <- run sess conn
      case res of
        Left err -> do
          putStrLn $ show err
          return Nothing
        Right result -> return $ Just result
