{-# LANGUAGE DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language TypeOperators, DataKinds #-}
module DocGraph.User where

import Data.Monoid ((<>))
import Data.Text (Text)
import Servant
import Control.Monad.IO.Class (liftIO)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Control.Monad (forM_, join)
import Web.FormUrlEncoded (FromForm, fromForm, parseUnique, parseMaybe)
import DocGraph.Bootstrap (formGroup, listGroupItem, applyHead)
import DocGraph.Document
import DocGraph.Database (runDB)
import Hasql.Query (Query, statement)
import Hasql.Session (query)
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
import Data.Functor.Contravariant (contramap)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Default
import GHC.Generics
import Servant.Auth.Server
import Data.Aeson


data User = User
  { userEmail      :: Text
  , userFullNames  :: Maybe Text
  , userName       :: Text
  , userPassword   :: Text
  } deriving (Show, Generic)

instance FromForm User where
  fromForm f = User
    <$> parseUnique "useremail"      f
    <*> parseMaybe  "userfullnames"  f
    <*> parseUnique "username"       f
    <*> parseUnique "userpassword"   f

instance Default (D.Row User) where
  def = User  <$> D.value          D.text
              <*> D.nullableValue  D.text
              <*> D.value          D.text
              <*> D.value          D.text

instance Default (E.Params User) where
  def = mconcat
      [ contramap userEmail      (E.value E.text)
      , contramap userFullNames  (E.nullableValue E.text)
      , contramap userName       (E.value E.text)
      , contramap userPassword   (E.value E.text)
      ]

instance ToJSON User
instance FromJSON User

instance ToJWT User
instance FromJWT User


getUserForm :: Handler CreateUserForm
getUserForm = return $ CreateUserForm Nothing

newtype CreateUserForm = CreateUserForm (Maybe User)
newtype ListUsers = ListUsers [User]

instance ToMarkup ListUsers where
  toMarkup (ListUsers user) = applyHead $ do
    H.h1 "List of users"
    mapM_ getUser user

getListUsersPage :: Handler ListUsers
getListUsersPage = ListUsers <$> liftIO selectUsers

selectUsers :: IO [User]
selectUsers = runDB $ query () q
  where
    q :: Query () [User]
    q = statement sql def def True

    sql = "select useremail, userfullnames, username, userpassword from users"

getUser :: User -> Html
getUser u =
  -- H.div ! A.class_ "list-group" $
  --   H.a ! A.href "#" ! A.class_ "list-group-item list-group-item-action flex-column align-items-start active" $ do
  --     H.div ! A.class_ "d-flex w-100 justify-content-between" $ do
  --       H.h5 ! A.class_ "mb-1" $ toHtml $ userName u
  --       H.small $ toHtml $ toHtml $ userName u
  --     H.p ! A.class_ "mb-1" $ toHtml $ userEmail u
  --     H.small $ toHtml $ toHtml $ userEmail u

   H.div ! A.class_ "card" ! A.style "width: 18rem;" $ do
     H.ul ! A.class_ "list-group list-group-flush" $
       H.li ! A.class_ "list-group-item" $ toHtml $ userName u

instance ToMarkup CreateUserForm where
  toMarkup (CreateUserForm muser) =
    applyHead $ do
      H.h1 "Create User"
      H.form ! A.action "/users" ! A.method "post" $ do
        formGroup "useremail"     "E-mail address"   (userEmail <$> muser)
        formGroup "userfullnames" "Full names"       (join $ userFullNames <$> muser)
        formGroup "username"  "Username"             (userName <$> muser)
        formGroup "userpassword"  "Password"         (userPassword <$> muser)
        H.button ! A.type_ "submit" ! A.class_ "btn btn-primary" $ "Submit"

storeUser :: User -> Handler Text
storeUser usr  = do
  liftIO $ insertUser usr
  return "User Created"

insertUser :: User -> IO ()
insertUser usr = runDB $ query usr q
  where
    q :: Query User ()
    q = statement sql def D.unit True

    sql = "insert into users(useremail, userfullnames, username, userpassword) values ($1, $2, $3, crypt('$4', gen_salt('bf')))"
