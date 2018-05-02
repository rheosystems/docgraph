
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph.Login where

import Data.Text (Text)
import Text.Blaze.Html5 as H
import Servant
import Data.Monoid ((<>))
import Text.Blaze.Html5.Attributes as A
import DocGraph.Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Hasql.Query (Query, statement)
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique, parseMaybe)
import Hasql.Session (Session, run, query)
import qualified Hasql.Encoders as E
import Data.Functor.Contravariant (contramap)
import qualified Hasql.Decoders as D
import Data.Default
import DocGraph.Bootstrap (applyHead)
import Data.Maybe (fromMaybe)
import Control.Monad (join, forM_)
import Data.Int (Int64)



data Login  = Login
  { userEmail       ::  Text
  , userPassword    ::  Text
  } deriving Show


instance FromForm Login where
  fromForm f = Login
    <$> parseUnique   "useremail"      f
    <*> parseUnique   "userpassword"   f
 --   <*> parseUnique   "userfullnames"  f


checkUser :: Login -> IO ()
checkUser user = runDB $ query user q
  where
    q :: Query Login ()
    q = statement sql encoder D.unit True

    sql = "select username, userfullnames from users where useremail = $1 and userpassword = $2"

    encoder :: E.Params Login
    encoder = contramap userEmail     (E.value E.text) <>
              contramap userPassword  (E.value E.text)


formGroup :: AttributeValue -> Html -> Maybe Text -> Html
formGroup fid ftitle mvalue =
  H.div ! A.class_ "form-group" $ do
    H.label ! for fid $ ftitle
    input ! A.type_ "text" ! A.class_ "form-control"
          ! A.id fid ! A.name fid
          ! A.value (maybe "" toValue mvalue)

data CreateLoginForm = CreateLoginForm (Maybe Login)

instance ToMarkup CreateLoginForm where
  toMarkup (CreateLoginForm mlog) =
    applyHead $ do
      H.h1 "User Login"
      let route = case mlog of
                    Nothing -> "/login?"
                    Just lo -> textValue $ "/login" <> userEmail lo
      H.form ! A.action route ! A.method "post" $ do
        formGroup "useremail"    "Email"    (userEmail <$> mlog)
        formGroup "userpassword" "Password" (userPassword <$> mlog)
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Login"

getLoginForm :: Handler CreateLoginForm
getLoginForm =
  return $ CreateLoginForm Nothing

datah su = "User found "

sendLogUser :: Login -> Handler Text
sendLogUser user = do
  liftIO $ checkUser user
  return  $ "user " <> (userEmail user) <> " found"


-- let r = case user of
--       Nothing -> 
-- return r
