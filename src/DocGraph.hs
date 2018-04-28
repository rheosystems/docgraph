
{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Monoid ((<>))
import Data.Text (Text)
import DocGraph.Document
import DocGraph.Project
import DocGraph.Login
import DocGraph.User
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.HTML.Blaze (HTML)
import Servant.Server (serve)


main :: IO ()
main = do
  putStrLn "Starting docgraph..."
  run 3000 $ logStdoutDev $ serve (Proxy :: Proxy Api) docgraph


type Api = Get '[HTML] DocumentForm
      :<|> "documents"
        :> Get '[HTML] ListPage
      :<|> "documents"
        :> ReqBody '[FormUrlEncoded] Document
        :> Post '[HTML] Text
      :<|> "document"
        :> Capture "reference" Text
        :> Get '[HTML] DocumentForm
      :<|> "document"
        :> Capture "reference" Text
        :> ReqBody '[FormUrlEncoded] Document
        :> Post '[HTML] Text
      :<|> "document"
        :> Capture "reference" Text
        :> "delete"
        :> Post '[HTML] Text
      :<|> "document"
        :> Capture "reference" Text
        :> "delete"
        :> Get '[HTML] DeleteDocumentForm
      :<|> "projects"
        :> Get '[HTML] ListProjectsPage
      :<|> "projects"
        :> "new"
        :> Get '[HTML] CreateProjectForm
      :<|> "projects"
        :> ReqBody '[FormUrlEncoded] Project
        :> Post '[HTML] Text
      :<|> "project"
        :> Capture "reference" Text
        :> Get '[HTML] UpdateProjectForm
      :<|> "project"
        :> Capture "reference" Text
        :> ReqBody '[FormUrlEncoded] Project
        :> Post '[HTML] Text
      :<|> "project"
        :> Capture "reference" Text
        :> "delete"
        :> Post '[HTML] Text
      :<|> "user"
        :> "new"
        :> Get '[HTML] CreateUserForm
      :<|> "user"
        :> ReqBody '[FormUrlEncoded] User
        :> Post '[HTML] Text
      :<|> "login"
        :> Get '[HTML] CreateLoginForm
      :<|> "login"
        :> ReqBody '[FormUrlEncoded] Login
        :> Post '[HTML] Text
      :<|> "static"
        :> Raw

docgraph :: Server Api
docgraph = getDocumentForm
      :<|> getListPage
      :<|> storeDocument
      :<|> getUpdateDocumentForm
      :<|> updateDocument
      :<|> deleteDocument
      :<|> getDeleteDocumentForm
      :<|> listProjects
      :<|> getProjectForm
      :<|> storeProject
      :<|> updateProjectForm
      :<|> updateProject
      :<|> deleteProject
      :<|> getUserForm
      :<|> storeUser
      :<|> getLoginForm
      :<|> sendLogUser
      :<|> serveDirectoryWebApp "static"
