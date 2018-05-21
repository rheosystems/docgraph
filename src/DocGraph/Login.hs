
{-# LANGUAGE TypeOperators, DataKinds #-}
module DocGraph.Login where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Blaze.Html5 as H
import Servant
import Data.Monoid ((<>))
import Text.Blaze.Html5.Attributes as A
import DocGraph.Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Hasql.Query (Query, statement)
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique, parseMaybe, lookupUnique, ToForm, toForm)
import Hasql.Session (Session, run, query)
import qualified Hasql.Encoders as E
import Data.Functor.Contravariant (contramap)
import qualified Hasql.Decoders as D
import Data.Default
import DocGraph.Bootstrap (applyHead)
import Data.Maybe (fromMaybe)
import Control.Monad (join, forM_)
import Data.Int (Int64)
import DocGraph.User
import Servant.Auth.Server


data Login = Login
  { loginEmail    :: Text
  , loginPassword :: Text
  } deriving Show

instance FromForm Login where
  fromForm f = Login
    <$> parseUnique "useremail"      f
    <*> parseUnique "userpassword"   f


checkUser :: Login -> IO (Maybe User)
checkUser user = runDB $ query user q
  where
    q :: Query Login (Maybe User)
    q = statement sql encoder (D.maybeRow def) True

    sql = "select useremail, userfullnames, username, userpassword from users where useremail = $1 and userpassword = $2"

    encoder :: E.Params Login
    encoder = contramap loginEmail     (E.value E.text) <>
              contramap loginPassword  (E.value E.text)



formGroup :: AttributeValue -> Html -> Maybe Text -> Html
formGroup fid ftitle mvalue =
  H.div ! A.class_ "form-group" $ do
    H.label ! for fid $ ftitle
    input ! A.type_ "text" ! A.class_ "form-control"
          ! A.id fid ! A.name fid
          ! A.value (maybe "" toValue mvalue)

newtype CreateLoginForm = CreateLoginForm (Maybe Login)

instance ToMarkup CreateLoginForm where
  toMarkup (CreateLoginForm mlog) =
    applyHead $ do
      H.h1 "User Login"
      let route = case mlog of
                    Nothing -> "/login?"
                    Just lo -> textValue $ "/login" <> loginEmail lo
      H.form ! A.action route ! A.method "post" $ do
        formGroup "useremail"    "Email"    (loginEmail <$> mlog)
        formGroup "userpassword" "Password" (loginPassword <$> mlog)
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Login"

getLoginForm :: Handler CreateLoginForm
getLoginForm =
  return $ CreateLoginForm Nothing

authenticate :: CookieSettings -> JWTSettings -> Login -> Handler (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Text)
authenticate cookieSettings jwtSettings login = do
  muser <- liftIO $ checkUser login
  case muser of
    Nothing   -> throwError err401
    Just user -> do
      mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
      case mApplyCookies of
        Nothing           -> throwError err401
        Just applyCookies -> return $ applyCookies "User Found"

