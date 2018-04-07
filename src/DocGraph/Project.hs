module DocGraph.Project where

import Control.Monad (forM_)
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


data ListProjectsPage = ListProjectsPage (Maybe [Project])

instance ToMarkup ListProjectsPage where
  toMarkup (ListProjectsPage mps) = do
    H.h1 "List of Projects"
    case mps of
      Nothing -> "nothing"
      Just ps -> H.ul $ mapM_ renderProject ps

renderProject :: Project -> Html
renderProject p = H.li $ toHtml $ projectRef p

data Project = Project
  { projectRef  :: Text
  , projectName :: Text
  } deriving Show

instance FromForm Project where
  fromForm f = Project
    <$> parseUnique "reference"      f
    <*> parseUnique "name" f

listProjects :: Handler ListProjectsPage
listProjects =
  return $ ListProjectsPage (Just [ Project "ref" "name"
                                  , Project "ref2" "name2"
                                  ])

data CreateProjectForm = CreateProjectForm

instance ToMarkup CreateProjectForm where
  toMarkup (CreateProjectForm) = do
    H.h1 "Create Project"
    H.form ! A.action "/projects" ! A.method "post" $ do
      formGroup "name"      "Name"
      formGroup "reference" "Reference"
      H.button ! A.type_ "submit" ! A.class_ "btn" $ "Submit"

getProjectForm :: Handler CreateProjectForm
getProjectForm =
  return CreateProjectForm

storeProject :: Project -> Handler Text
storeProject pro = do
  liftIO $ insertProject pro
  return "received"

insertProject :: Project -> IO (Maybe ())
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

formGroup :: AttributeValue -> Html -> Html
formGroup fid ftitle  =
  H.div ! A.class_ "form-group" $ do
    H.label ! for fid $ ftitle
    input ! A.type_ "text" ! A.class_ "form-control" ! A.id fid ! A.name fid
