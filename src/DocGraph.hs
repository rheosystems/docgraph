{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Monoid ((<>))
import Data.Text (Text)
import DocGraph.Project (ListProjectsPage, UpdateProjectForm, CreateProjectForm(..), getProjectForm, listProjects, storeProject,updateProjectForm, Project)
import DocGraph.Pages (SubmitPage(SubmitPage), ListPage(ListPage))
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Blaze (HTML)
import Servant.Server (serve)
import DocGraph.Database
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
      :<|> "projects"
        :> Get '[HTML] ListProjectsPage
      :<|> "projects"
        :> "new"
        :> Get '[HTML] CreateProjectForm
      :<|> "projects"
        :> ReqBody '[FormUrlEncoded] Project
        :> Post '[HTML] Text
      :<|> "projects"
        :> Capture "reference" Text
        :> Get '[HTML] UpdateProjectForm
      :<|> "projects"
        :> Capture "reference" Text
        :> ReqBody '[FormUrlEncoded] Project
        :> Post '[HTML] Text
      :<|> "static" :> Raw
docgraph :: Server Api
docgraph = getSubmitPage
      :<|> getListPage
      :<|> storeDocument
      :<|> listProjects
      :<|> getProjectForm
      :<|> storeProject
      :<|> updateProjectForm
      :<|> undefined
      :<|> serveDirectoryWebApp "static"





storeDocument :: Document -> Handler Text
storeDocument doc  = do
  mres <- liftIO $ insertDocument doc
  return $ case mres of
    Nothing -> "Something went wrong with runDB"
    Just i -> "Received: "

getSubmitPage :: Handler SubmitPage
getSubmitPage = return SubmitPage

getListPage :: Handler ListPage
getListPage = do
  mdocs <- liftIO $ selectDocuments
  return $ ListPage mdocs
