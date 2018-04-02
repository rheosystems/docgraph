module DocGraph.Project where

import Data.Text (Text)
import Control.Monad (forM_)
import Servant
import Text.Blaze.Html5 as H

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

listProjects :: Handler ListProjectsPage
listProjects =
  return $ ListProjectsPage (Just [ Project "ref" "name"
                                  , Project "ref2" "name2"
                                  ])
