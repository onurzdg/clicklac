
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Clicklac.User
  (
  -- * Data Types
    Account(..)
  , AccountState(..)
  , AccountStateUpdate(..)
  , Profile
  , NewAccount

  -- * DB Ops
  , getUserAccById
  , newAccount
  , updateProfile
  , getUserIdByEmail
  , getUserIdByUsername
  , updateAccountState
  , updateLastUserActivity
  ) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.State
  ( State
  , execState
  , modify
  )
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
  ( ExceptT (..)
  , runExceptT
  , throwE
  )
import Data.Functor.Identity (Identity (..))
import Data.Time.Clock (UTCTime (..))

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , parseJSON
  , toJSON
  , (.:)
  , (.:!)  
  , (.=)
  )
import Data.Aeson.Types
  ( Value (Object, String)
  , typeMismatch
  )
import qualified Database.CQL.IO as CQ
  ( BatchM
  , ClientState
  , PrepQuery
  , addPrepQuery
  , batch
  , prepared
  , query1
  , runClient
  , setType
  , trans
  , write
  )
import Database.CQL.Protocol
  ( BatchType (BatchLogged)
  , ColumnType (TextColumn)
  , Cql (..)
  , QueryParams
  , R
  , Record (..)
  , Row
  , Tagged (..)
  , TupleType
  , Value (CqlText)
  , W
  , recordInstance
  , rowLength
  )
import Database.CQL.Protocol.Extra
 ( defQueryParams
 , defQueryParamsMeta
 )

import Clicklac.OpFailure (FailureMsg (..))
import Clicklac.Session (SessionId)
import Clicklac.Types
import qualified Clicklac.Types.Password as PW
import Clicklac.Validation
  ( OpValidation
  , Validatable (..)
  )

type ActivatedAt = UTCTime
type CreatedAt   = UTCTime
type UpdatedAt   = UTCTime
type WebUrl = Url Validated
type AvatarUrl = Url Validated

generateUserId :: (UUIDGenerator m) => m UserId
generateUserId = fmap UserId generateUUIDv4

------- User Op Failure Reasons ----------------------------
data UserOpFailure
  = EmailExists
  | UsernameExists
  | UserNotFound
  | OldPasswordWrong
  deriving (Eq, Show)

instance FailureMsg UserOpFailure where
  failMsg EmailExists      = "Email already exists"
  failMsg UsernameExists   = "Username already exists"
  failMsg UserNotFound     = "User not found"
  failMsg OldPasswordWrong = "Old password is wrong"

-------- AccountState --------------------------------------
data AccountState
  = Active
  | Suspended
  deriving (Show, Eq)

data AccountStateUpdate
  = ASU AccountState

instance FromJSON AccountStateUpdate where
  parseJSON (Object o) = ASU <$> o .: "state"
  parseJSON u = typeMismatch "AccountStateUpdate" u

instance ToJSON AccountState where
  toJSON Active = "active"
  toJSON Suspended = "suspended"

instance FromJSON AccountState where
  parseJSON (String "active") = pure Active
  parseJSON (String "suspended") = pure Suspended
  parseJSON u = typeMismatch "Expected AccountState" u

instance Cql AccountState where
  ctype = Tagged TextColumn
  toCql Active  = CqlText "active"
  toCql Suspended = CqlText "suspended"
  fromCql (CqlText "active")     = Right Active
  fromCql (CqlText "suspended")  = Right Suspended
  fromCql _                      = Left "AccountState: Expected CqlText"

recordInstance ''AccountStateUpdate

--------Account---------------------------------------
data Account = Account
  { accUid           :: !UserId
  , accUsername      :: !(Username Validated)
  , accName          :: !(Name Validated)
  , accEmail         :: !(Email Validated)
  , accPassword      :: !EncryptedPass
  , accBio           :: !(Maybe (Bio Validated))
  , accUrl           :: !(Maybe (Url Validated))
  , accAvatarUrl     :: !(Maybe (Url Validated))
  , accLocation      :: !(Maybe (Location Validated))
  , accActivatedAt   :: !UTCTime
  , accSuspendededAt :: !(Maybe UTCTime)
  , accCreatedAt     :: !UTCTime
  , accUpdatedAt     :: !(Maybe UTCTime)
  , accState         :: !AccountState
  } deriving (Show)

instance ToJSON Account where
  toJSON Account {..} = object [
    "uid"  .= accUid,  "username" .= accUsername,
    "name" .= accName, "email"    .= accEmail,
    "bio"  .= accBio,  "webUrl"   .= accUrl, "state" .= accState,
    "avatarUrl" .= accAvatarUrl, "createdAt" .= accCreatedAt,
    "location"  .= accLocation,  "updatedAt" .= accUpdatedAt ]

recordInstance ''Account

---------- New Account ------------------
data NewAccount a = NewAccount
  (Username a)
  (Email a)
  (ClearTextPass a)
  (Name a)
  (Maybe (Bio a))
  (Maybe (Url a))
  (Maybe (Url a))
  (Maybe (Location a)) 
  
type AccCreationTuple =
    (UserId, Username Validated, Name Validated, Email Validated,
     EncryptedPass, Maybe (Bio Validated), Maybe WebUrl, Maybe AvatarUrl,
     Maybe (Location Validated), ActivatedAt, CreatedAt, AccountState)

validateNewAccount :: NewAccount Unvalidated
                   -> OpValidation (NewAccount Validated)
validateNewAccount (NewAccount uname email pass name' mbio url' avaUrl loc) =
  NewAccount
    <$> validate uname
    <*> validate email
    <*> validate pass
    <*> validate name'
    <*> validate mbio
    <*> validate url'
    <*> validate avaUrl
    <*> validate loc

instance FromJSON (OpValidation(NewAccount Validated)) where
  parseJSON (Object o) = validateNewAccount <$>
    (NewAccount
      <$> o .: "username"
      <*> o .: "email"
      <*> o .: "password"
      <*> o .: "name"
      <*> o .:! "bio"
      <*> o .:! "webUrl"
      <*> o .:! "avatarUrl"
      <*> o .:! "location"
    ) 
  parseJSON unknown = typeMismatch "NewAccount" unknown

instance Show (NewAccount a) where
  show (NewAccount u e _ n b url' avaUrl loc) = "NewAccountU {" ++
    "username = " ++ show u   ++ ", " ++ "name =" ++ show n ++ ", " ++
    "email = "  ++ show e   ++ ", " ++ "bio = " ++ show b ++ ", " ++
    "webUrl = " ++ show url' ++ ", " ++ "avatarUrl = " ++ show avaUrl ++ ", " ++
    "location = " ++ show loc ++ "}"

--------Profile---------------------------------------
data Profile a = Profile
  { pfUsername  :: !(Username a)
  , pfEmail     :: !(Email a)
  , pfName      :: !(Name a)
  , pfBio       :: !(Maybe (Bio a))
  , pfWebUrl    :: !(Maybe (Url a))
  , pfAvatarUrl :: !(Maybe (Url a))
  , pfLocation  :: !(Maybe (Location a))
  } deriving (Show)

validateProfile :: Profile Unvalidated
                -> OpValidation (Profile Validated)
validateProfile (Profile uname email name' mbio mwebUrl mavatarUrl mloc) =
  Profile
    <$> validate uname
    <*> validate email
    <*> validate name'
    <*> validate mbio
    <*> validate mwebUrl
    <*> validate mavatarUrl
    <*> validate mloc

instance FromJSON (OpValidation (Profile Validated)) where
  parseJSON (Object o) = validateProfile <$>
    (Profile 
      <$> o .:  "username"
      <*> o .:  "email"
      <*> o .:  "name"
      <*> o .:! "bio"
      <*> o .:! "webUrl"
      <*> o .:! "avatarUrl"
      <*> o .:! "location"
    ) 
  parseJSON unknown = typeMismatch "Profile" unknown

instance ToJSON (Profile Validated) where
  toJSON Profile {..} = object [
    "username" .= pfUsername,
    "name" .= pfName,  "email"  .= pfEmail,
    "bio"  .= pfBio,  "webUrl"  .= pfWebUrl, 
    "avatarUrl" .= pfAvatarUrl, "location" .= pfLocation]

----------------- DB OPs ------------------------------
updateProfile :: (CassClient m, TimeGetter m)
              => UserId
              -> Profile Validated
              -> m (Either [UserOpFailure] (Profile Validated))
updateProfile uid prof@Profile{..} = do
  macc <- getUserAccById uid
  case macc of
    Nothing -> return $ Left [UserNotFound]
    Just Account{..} -> do
      let curEmail = accEmail
          curUname = accUsername
          changeEmail = pfEmail /= curEmail
          changeUname = pfUsername /= curUname
      dbres <- runExceptT $
        updateEmailAndUsername changeEmail changeUname pfEmail pfUsername
      case dbres of
        (Right _) -> do
          curT <- getCurrentTime
          let record = (pfUsername, pfName, pfEmail,
                pfBio, pfWebUrl, pfAvatarUrl, pfLocation, curT, uid)
          updateProfile' (CQ.addPrepQuery qUpdateAcc record)
              changeEmail changeUname curEmail curUname
          return $ Right prof
        Left e@EmailExists ->
           return $ Left [e]
        Left u@UsernameExists -> do
           when changeEmail $ deleteEmail pfEmail
           return $ Left [u]
        _ -> error "Impossible happened in updateProfile"

 where
   updateProfile' :: CassClient m
                  => CQ.BatchM ()
                  -> Bool
                  -> Bool
                  -> Email Validated
                  -> Username Validated
                  -> m ()
   updateProfile' updateOp ce cu email uname =
     runCassOp $ CQ.batch $
       execState (batchS ce cu email uname)
         (CQ.setType BatchLogged >> updateOp)
    where
      batchS :: Bool
             -> Bool
             -> Email Validated
             -> Username Validated
             -> State (CQ.BatchM ()) ()
      batchS changeEmail changeUname prevEmail prevUname = do
        when changeEmail $ modify
          (>> CQ.addPrepQuery qDelEmail (Identity prevEmail) )
        when changeUname $ modify
          (>> CQ.addPrepQuery qDelUsername (Identity prevUname) )

   updateEmailAndUsername changeEmail changeUname newEmail newUname =
        execState (update changeEmail changeUname newEmail newUname) (return ())
     where
       update :: CassClient m
              => Bool
              -> Bool
              -> Email Validated
              -> Username Validated
              -> State (ExceptT UserOpFailure m ()) ()
       update changeE changeU newE newU = do
         when changeE $ modify
           (>> insertEmail newE uid)
         when changeU $ modify
           (>> insertUsername newU uid)

   qUpdateAcc :: CQ.PrepQuery W (Username Validated, Name Validated,
       Email Validated, Maybe (Bio Validated), Maybe WebUrl, Maybe AvatarUrl,
       Maybe (Location Validated), UpdatedAt, UserId) ()
   qUpdateAcc = CQ.prepared
     "update user_account set user_name =?, name =?,\
      \ email =?, bio =?, url =?, avatar_url =?, location =?, \
      \ updated_at = ? where id =?"

   qDelEmail :: CQ.PrepQuery W (Identity (Email Validated)) ()
   qDelEmail = CQ.prepared
     "delete from login_by_email where email = ?"

   qDelUsername :: CQ.PrepQuery W (Identity (Username Validated)) ()
   qDelUsername = CQ.prepared
     "delete from login_by_user_name where user_name = ?"

deleteUsername :: CassClient m => Username Validated -> m ()
deleteUsername uname = runCassOp $ CQ.write q p
 where
   q :: CQ.PrepQuery W (Identity (Username Validated)) ()
   q = CQ.prepared "delete from login_by_user_name where user_name = ?"

   p :: QueryParams (Identity (Username Validated))
   p = defQueryParams (Identity uname)

deleteEmail :: CassClient m => Email Validated -> m ()
deleteEmail email = runCassOp $ CQ.write q p
 where
   q :: CQ.PrepQuery W (Identity (Email Validated)) ()
   q = CQ.prepared "delete from login_by_email where email = ?"

   p :: QueryParams (Identity (Email Validated))
   p = defQueryParams (Identity email)

updateLastUserActivity :: MonadIO m
                       => CQ.ClientState
                       -> UserId
                       -> SessionId -> m ()
updateLastUserActivity s uid sid = CQ.runClient s $ CQ.write q p
 where
   q :: CQ.PrepQuery W (SessionId, UserId) ()
   q = CQ.prepared "update user_last_activity set session_id = ?, \
           \ last_activity_at = toTimestamp(now()) where user_id = ? "

   p :: QueryParams (SessionId, UserId)
   p = defQueryParams (sid, uid)

insertUsername :: Username Validated -> UserId -> CQErr UserOpFailure ()
insertUsername uname uid = do
  [transRes] <- lift . runCassOp $ CQ.trans q p
  unless (rowLength transRes == 1) $ throwE UsernameExists

 where
   q :: CQ.PrepQuery W (Username Validated, UserId) Row
   q = CQ.prepared "insert into login_by_user_name(user_name, user_id) \
                      \ values(?, ?) if not exists"

   p :: QueryParams (Username Validated, UserId)
   p = defQueryParamsMeta (uname, uid)

insertEmail :: Email Validated -> UserId -> CQErr UserOpFailure ()
insertEmail e uid = do
  [transRes] <- lift . runCassOp $ CQ.trans q p
  unless (rowLength transRes == 1) $ throwE EmailExists
 where
   q :: CQ.PrepQuery W (Email Validated, UserId) Row
   q = CQ.prepared "insert into login_by_email(email, user_id) \
                     \ values(?, ?) if not exists"

   p :: QueryParams (Email Validated, UserId)
   p = defQueryParamsMeta (e, uid) -- meta info is needed as it is a LWT

getUserAccById :: CassClient m => UserId -> m (Maybe Account)
getUserAccById userid = fmap asRecord <$> runCassOp (CQ.query1 q p)
 where
   q :: CQ.PrepQuery R (Identity UserId) (TupleType Account)
   q = CQ.prepared "select id, user_name, name, email, password, bio, url, \
        \ avatar_url, location, activated_at, suspended_at, created_at, \
        \ updated_at, state \
        \ from user_account where id = ?"

   p :: QueryParams (Identity UserId)
   p = defQueryParams (Identity userid)

updateAccountState :: CassClient m => UserId -> AccountStateUpdate -> m ()
updateAccountState uid (ASU state) = runCassOp $ CQ.write (q state) p
 where
   q :: AccountState -> CQ.PrepQuery W (AccountState, UserId) ()
   q Suspended =
     CQ.prepared "update user_account set state =?, \
                  \ suspended_at = toTimestamp(now()) where id =?"
   q _ = CQ.prepared "update user_account set state =? where id =?"

   p :: QueryParams (AccountState, UserId)
   p = defQueryParams (state, uid)

getUserIdByEmail :: CassClient m
                 => Email Validated
                 -> m (Maybe UserId)
getUserIdByEmail email = fmap runIdentity <$> runCassOp (CQ.query1 q p)
 where
   q :: CQ.PrepQuery R (Identity (Email Validated)) (Identity UserId)
   q = CQ.prepared "select user_id from login_by_email where email = ?"

   p :: QueryParams (Identity (Email Validated))
   p = defQueryParams (Identity email)

getUserIdByUsername :: CassClient m
                    => Username Validated
                    -> m (Maybe UserId)
getUserIdByUsername uname = fmap runIdentity <$> runCassOp (CQ.query1 q p)
 where
   q :: CQ.PrepQuery R (Identity (Username Validated)) (Identity UserId)
   q = CQ.prepared "select user_id from login_by_user_name where user_name = ?"

   p :: QueryParams (Identity (Username Validated))
   p =  defQueryParams (Identity uname)

newAccount :: (CassClient m, TimeGetter m, UUIDGenerator m, PasswordEncryptor m)
           => NewAccount Validated
           -> m (Either [UserOpFailure] Account)
newAccount (NewAccount uname email cp name' mbio murl mAvaUrl mloc) = do
  uid <- generateUserId
  emailRes <- runExceptT $ insertEmail email uid
  unameRes <- runExceptT $ insertUsername uname uid
  createAcc emailRes unameRes uid
 where
   createAcc (Right _) (Right _) uid = do
     curT <- getCurrentTime
     ep <- PW.toEncrypted cp
     runCassOp $ CQ.write qAcc (pAcc (uid, uname, name',
       email, ep, mbio, murl, mAvaUrl, mloc, curT, curT, Active))
     let acc =  Account uid uname name' email ep 
                 mbio murl mAvaUrl mloc curT Nothing curT Nothing Active
     return $ Right acc
   createAcc (Left emailExists) (Right _) _ = do
     deleteUsername uname
     return $ Left [emailExists]
   createAcc (Right _) (Left unameExists) _ = do
     deleteEmail email
     return $ Left [unameExists]
   createAcc (Left e) (Left u) _ = return $ Left [e, u]

   qAcc :: CQ.PrepQuery W AccCreationTuple ()
   qAcc = CQ.prepared
     "insert into user_account \
     \ (id, user_name, name, email, password, \
     \ bio, url, avatar_url, location, activated_at, \
     \ created_at, state) \
     \ values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"

   pAcc :: AccCreationTuple -> QueryParams AccCreationTuple
   pAcc = defQueryParams
   
