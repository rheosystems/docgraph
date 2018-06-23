module DocGraph.Project where

import Data.Text (Text)
import Servant
import DocGraph.Database (runDB)
import Hasql.Query (Query, statement)
import Hasql.Session (Session, run, query)
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique, parseMaybe)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Data.Monoid ((<>))
import Data.Functor.Contravariant (contramap)
import Control.Monad (join, forM_)
import Data.Maybe (fromMaybe)
import DocGraph.User (User(..))
import Servant.Auth.Server (AuthResult(..))


data ListProjectsPage = ListProjectsPage User [Project]

instance ToMarkup ListProjectsPage where
  toMarkup (ListProjectsPage user ps) = do
    let name = fromMaybe (userName user) (userFullNames user)
    H.h1 "List of Projects"
    H.p $ "Belonging to: " <> toHtml name
    H.a ! A.href "/projects/new" $ "create project"
    H.ul $ mapM_ renderProject ps

renderProject :: Project -> Html
renderProject p = let ref = projectRef p in
  H.li $ H.a ! A.href (textValue $ "/project/" <> ref) $ toHtml ref

data Project = Project
  { projectRef  :: Text
  , projectName :: Text
  } deriving Show

instance FromForm Project where
  fromForm f = Project
    <$> parseUnique "reference"      f
    <*> parseUnique "name"           f

listProjects :: AuthResult User -> Handler ListProjectsPage
listProjects (Authenticated user) =
  ListProjectsPage user <$> liftIO (selectProjects $ userEmail user)
listProjects _ = throwError err401

selectProjects :: Text -> IO [Project]
selectProjects email = runDB $ query email q
  where
    q :: Query Text [Project]
    q = statement sql (E.value E.text) decoder True

    sql = "select reference, name \
          \from projects \
          \join project_users \
          \on projects.reference = project_users.project_reference \
          \where project_users.user_email = $1"

    decoder :: D.Result [Project]
    decoder = D.rowsList $ Project <$> D.value D.text <*> D.value D.text

data CreateProjectForm = CreateProjectForm

instance ToMarkup CreateProjectForm where
  toMarkup CreateProjectForm = do
    H.h1 "Create Project"
    H.form ! A.action "/projects" ! A.method "post" $ do
      formGroup "name"      "Name"      Nothing
      formGroup "reference" "Reference" Nothing
      H.button ! A.type_ "submit" ! A.class_ "btn" $ "Submit"
      H.a ! A.href "/projects" $ "List Projects"

getProjectForm :: Handler CreateProjectForm
getProjectForm =
  return CreateProjectForm

storeProject :: Project -> Handler Text
storeProject pro = do
  liftIO $ insertProject pro
  return "received"

insertProject :: Project -> IO ()
insertProject proj = runDB $ query proj q
  where
    q :: Query Project ()
    q = statement sql encoder decoder True

    sql = "insert into projects (reference, name) values ($1,$2)"

    encoder :: E.Params Project
    encoder = contramap projectRef  (E.value E.text) <>
              contramap projectName (E.value E.text)

    decoder :: D.Result ()
    decoder = D.unit

formGroup :: AttributeValue -> Html -> Maybe Text -> Html
formGroup fid ftitle mvalue =
  H.div ! A.class_ "form-group" $ do
    H.label ! for fid $ ftitle
    input ! A.type_ "text" ! A.class_ "form-control"
          ! A.id fid ! A.name fid ! A.value (maybe "" toValue mvalue)

newtype UpdateProjectForm = UpdateProjectForm (Maybe Project)

updateProjectForm :: Text -> Handler UpdateProjectForm
updateProjectForm ref = UpdateProjectForm <$> liftIO (selectProject ref)

updateProject :: Text -> Project -> Handler Text
updateProject ref p = do
  liftIO $ runDB $ query (ref, projectName p) q
  return "project updated"
  where
    q :: Query (Text, Text) ()
    q = statement sql encoder D.unit True

    sql = "update projects set name = $2 where reference = $1"

    encoder :: E.Params (Text, Text)
    encoder = contramap fst (E.value E.text) <>
              contramap snd (E.value E.text)

selectProject :: Text -> IO (Maybe Project)
selectProject reff = runDB $ query reff q
  where
    q :: Query Text (Maybe Project)
    q = statement sql encoder decoder True

    sql = "select reference, name from projects where reference = $1"

    encoder :: E.Params Text
    encoder = E.value E.text

    decoder :: D.Result (Maybe Project)
    decoder = D.maybeRow (Project <$> D.value D.text
                                   <*> D.value D.text)

instance ToMarkup UpdateProjectForm where
  toMarkup (UpdateProjectForm Nothing) = H.h1 "Project not found"
  toMarkup (UpdateProjectForm (Just p)) = do
    let ref = projectRef p
    H.h1 "Update Project"
    H.form ! A.action ("/project/" <> toValue ref) ! A.method "post" $ do
      formGroup "name"      "Name"      (Just $ projectName p)
      formGroup "reference" "Reference" (Just ref)
      H.button ! A.type_ "submit" ! A.class_ "btn" $ "Submit"
      H.a ! A.href "/projects" $ "List Projects"
    H.form ! A.action ("/project/" <> toValue ref <> "/delete") ! A.method "post" $
      H.button ! A.type_ "Submit" $ "Delete Project"

deleteProject :: Text -> Handler Text
deleteProject ref = do
  liftIO $ eraseProject ref
  return "Project Deleted."

eraseProject :: Text -> IO ()
eraseProject ref = runDB $ query ref q
  where
  q :: Query Text ()
  q = statement sql encoder decoder True

  sql = "delete from projects where reference = $1"

  decoder :: D.Result ()
  decoder = D.unit

  encoder :: E.Params Text
  encoder = E.value E.text
