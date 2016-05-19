{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Clicklac.API.Authentication
  ( authAPI
  , AuthAPI
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.TH.Extra (prefixRemovedOpts)
import Data.Text (Text)
import Servant

import Clicklac.Application (App)
import Clicklac.Cookie
  ( CookieHeader
  , cookieHeader
  , expiredCookieHeader
  , sessionIdCookieKey
  )
import Clicklac.OpFailure (FailureMsg (..))
import Clicklac.Servant.Combinator
  ( AuthProtected
  , NotAuthProtected
  , WithUserId
  )
import qualified Clicklac.Servant.Response as RS
import Clicklac.Session
  ( Session
  , SessionId
  , createSession
  , deleteSession
  , sessionIdT
  , ssEndAt
  , ssId
  )
import Clicklac.Types
import qualified Clicklac.Types.Password as PW
import Clicklac.User
  ( Account (..)
  , AccountState (..)
  , getUserAccById
  , getUserIdByEmail
  , getUserIdByUsername
  )

data UserLoginFailure
  = InvalidLoginId
  | UserIdNotFound
  | WrongPassword
  | AlreadyAuthenticated
  | AccountSuspended
  | AuthRequired
  deriving (Show, Eq)

instance FailureMsg UserLoginFailure where
  failMsg InvalidLoginId = "Invalid login id"
  failMsg UserIdNotFound = "User id not found"
  failMsg WrongPassword = "Wrong password"
  failMsg AlreadyAuthenticated  = "Already authenticated"
  failMsg AccountSuspended = "Your account is suspended"
  failMsg AuthRequired = "Authentication required"

data Login = Login
  { _lgUserId   :: !Text
  , _lgPassword :: !(Password 'PWUnvalidated)
  } deriving (Show)

deriveJSON  (prefixRemovedOpts 3) ''Login

type AuthAPI = "auth" :> AuthAPI'
type AuthAPI' =
    NotAuthProtected :> ReqBody '[JSON] Login
    :> Header "X-Forwarded-For" Text
    :> PostCreated '[JSON]
    (Headers '[Header "Set-Cookie" CookieHeader] Session)
  :<|>
    AuthProtected :> WithUserId :> Header "Authorization" SessionId
    :> DeleteNoContent '[JSON] (Headers '[Header "Set-Cookie" CookieHeader] ())

authAPI :: ServerT AuthAPI App
authAPI = login :<|> logout

login :: Login
      -> Maybe Text
      -> App (Headers '[Header "Set-Cookie" CookieHeader] Session)
login l t = do
  authRes <- authenticateUser l t
  case authRes of
    Right sess -> do
      let cookieBS = cookieHeader sessionIdCookieKey
                     (TE.encodeUtf8 . sessionIdT . ssId $ sess)
                     (ssEndAt sess)
      return $ addHeader cookieBS sess
    Left err -> RS.s401 (T.pack "Failed to authenticate") [err]

logout :: UserId
       -> Maybe SessionId
       -> App (Headers '[Header "Set-Cookie" CookieHeader] ())
logout uid (Just sessId) = do
  deleteSession uid sessId
  return $ addHeader (expiredCookieHeader sessionIdCookieKey) ()
logout _ Nothing = RS.s401 (T.pack "Failed operation") [AuthRequired]

authenticateUser :: Login
                 -> Maybe Text
                 -> App (Either UserLoginFailure Session)
authenticateUser (Login email pw) remoteIp
  | Just emailv <- validateEmail (EmailU email) = do
      macc <- runMaybeT $ (MaybeT . getUserAccById) =<<
                          (MaybeT $ getUserIdByEmail emailv)
      maybe (return $ Left UserIdNotFound)
            (\acc -> createSession' acc pw remoteIp) macc
authenticateUser (Login uname pw) remoteIp
  | Just unamev <- validateUsername (UsernameU uname) = do
      macc <- runMaybeT $ (MaybeT . getUserAccById) =<<
                          (MaybeT $ getUserIdByUsername unamev)
      maybe (return $ Left UserIdNotFound)
            (\acc -> createSession' acc pw remoteIp) macc
authenticateUser _ _ =  return $ Left InvalidLoginId

createSession' :: Account
               -> Password 'PWUnvalidated
               -> Maybe Text
               -> App (Either UserLoginFailure Session)
createSession' acc pw remoteIp
  | accState acc == Active =
    if verifyPass pw (accPassword acc)
      then Right `fmap` createSession (accUid acc) remoteIp
      else return $ Left WrongPassword
  | otherwise = return $ Left AccountSuspended
 where
   verifyPass text enc =
     case PW.clearTextPass text of
       Right cp -> PW.verify cp enc
       Left _ -> False
