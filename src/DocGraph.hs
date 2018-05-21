{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph where

import Data.Text (Text)
import DocGraph.Document
import DocGraph.Project
import DocGraph.Login
import DocGraph.User
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant hiding (NotSecure)
import Servant.HTML.Blaze (HTML)
import Servant.Server (serve)
import Control.Monad.IO.Class (liftIO)
import Servant.Auth.Server



main :: IO ()
main = do
  putStrLn "Starting docgraph..."
  -- FIXME: key below should be read from file otherwise generated
  -- each time server starts and invalidates existing browser cookies
  myKey <- generateKey
  let jwt = defaultJWTSettings myKey
      -- FIXME: not enabling secure and excluding xsrf is done for
      -- development and must be fixed before production deployment
      cookie = defaultCookieSettings { cookieIsSecure = NotSecure, xsrfExcludeGet = True }
      ctx = cookie :. jwt :. EmptyContext
  run 3000 $ logStdoutDev $ serveWithContext (Proxy :: Proxy Api) ctx (docgraph cookie jwt)


type Api = Get '[HTML] DocumentForm
      :<|> "documents"
        :> Get '[HTML] ListPage
      :<|> "documents.json"
        :> Get '[JSON] [Document]
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
      :<|> Auth '[Cookie] User
        :> "projects"
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
      :<|> "users"
        :> Get '[HTML] CreateUserForm
      :<|> "users"
        :> ReqBody '[FormUrlEncoded] User
        :> Post '[HTML] Text
      :<|> "userlist"
        :> Get '[HTML] ListUsers
      :<|> "login"
        :> Get '[HTML] CreateLoginForm
      :<|> "login"
        :> ReqBody '[FormUrlEncoded] Login
        :> Post '[HTML] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)
      :<|> "static"
        :> Raw

docgraph :: CookieSettings -> JWTSettings -> Server Api
docgraph cookie jwt = getDocumentForm
      :<|> getListPage
      :<|> liftIO selectDocuments
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
      :<|> getListUsersPage
      :<|> getLoginForm
      :<|> authenticate cookie jwt
      :<|> serveDirectoryWebApp "static"
