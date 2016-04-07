{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}    
{-# LANGUAGE GADTs               #-}    
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}    
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StandaloneDeriving  #-}        
{-# LANGUAGE TemplateHaskell     #-}    
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}    
    
module Clicklac.User
  (
  -- * Data Types
    Account(..)
  , AccountState(..)
  , AccountStateUpdate(..)
  , Bio(BioU)
  , Username(UsernameU)
  , Profile
  , NewAccount
  , InitState(..)
  
  -- * Accessors
  , locationT
  , usernameT
  , nameT
  , bioT
  
  -- * New Account Accessors
  , newAccUserId
  , newAccUsername
  , newAccEmail
  , newAccPassword
  , newAccPassword'
  , newAccName
  , newAccBio
  , newAccWebUrl
  , newAccAvatarUrl
  , newAccLocation
  , newAccActivatedAt
  , newAccCreatedAt

  -- * Validation
  , validateUsername

  -- * DB Ops
  , getUserAccById
  , newAccount
  , updateProfile
  , getUserIdByEmail
  , getUserIdByUsername
  , updateAccountState
  , updateLastUserActivity
  ) where        

import Data.Char (isAlphaNum)       
import Data.Functor.Identity (Identity (..))
import Data.Maybe (fromJust)       
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day(..))        
import Data.Time.Clock (UTCTime(..))       
import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (throwE, runExceptT, ExceptT(..))       
import Control.Monad.State.Strict (modify, execState, State)
       
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , toJSON
  , parseJSON
  , (.=)
  , (.:)
  , (.:?)
  , object
  )
import Data.Aeson.TH (deriveToJSON)       
import Data.Aeson.TH.Extra (prefixRemovedOpts)              
import Data.Aeson.Types (Value(String, Object), typeMismatch)
import qualified Database.CQL.IO as CQ
  ( BatchM
  , PrepQuery
  , ClientState
  , runClient
  , addPrepQuery
  , batch
  , prepared
  , query1
  , setType
  , trans
  , write
  )       
import Database.CQL.Protocol
  ( Cql(..)
  , BatchType (BatchLogged)
  , Tagged(..)
  , ColumnType (TextColumn)
  , Value(CqlText)
  , TupleType
  , QueryParams
  , Record(..)
  , Row
  , W
  , R
  , recordInstance
  , rowLength
  )
import Database.CQL.Protocol.Extra
  ( defQueryParams
  , defQueryParamsMeta
  )
import qualified Data.UUID as UUID  
import Servant.Docs  
       
import Clicklac.Email (Email(EmailU), validateEmail)
import Clicklac.OpFailure (FailureMsg (..))       
import Clicklac.Validation
  ( Validatable (..)
  , ValidationFailure (InvalidUsername, InvalidName, InvalidBio)
  , success
  , failure
  , OpValidation
  , Validation(..)
  )
import Clicklac.Url (Url(UrlU), validateUrl)
import Clicklac.Session (SessionId)       
import Clicklac.Password
  ( PWState(..)
  , Password(..)
  , encrypt'
  )       
import qualified Clicklac.Password as PW       
import Clicklac.Types
  ( CassClient(..) 
  , TimeGetter(..) 
  , UUIDGenerator(..) 
  , PasswordEncryptor(..) 
  , UserId(..)
  , ValidationState(..)
  , CQErr
  , generateUserId
  )

type ActivatedAt = UTCTime
type CreatedAt   = UTCTime 
type UpdatedAt   = UTCTime
type WebUrl = Url 'Validated
type AvatarUrl = Url 'Validated

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
         
instance FromJSON AccountState where
  parseJSON (String "active") = pure Active
  parseJSON (String "suspended") = pure Suspended
  parseJSON u = typeMismatch "Expected AccountState" u

instance Cql AccountState where
  ctype = Tagged TextColumn
  toCql (Active)    = CqlText "active"
  toCql (Suspended) = CqlText "suspended"
  fromCql (CqlText "active")     = Right Active
  fromCql (CqlText "suspended")  = Right Suspended
  fromCql _                      = Left "AccountState: Expected CqlText"   

recordInstance ''AccountStateUpdate

-- Docs

instance ToJSON AccountState where
  toJSON Active = String "active"
  toJSON Suspended = String "suspended"            

instance ToJSON AccountStateUpdate where
  toJSON (ASU s) = object ["state" .= s]
  
instance ToSample AccountStateUpdate where  
  toSamples _ = singleSample $ ASU Active   

--------- Location ----------------------------------------
data Location :: ValidationState -> * where 
     LocV :: Text -> Location 'Validated
     LocU :: Text -> Location 'Unvalidated

locationT :: Location a -> Text
locationT (LocV l) = l
locationT (LocU l) = l              

deriving instance Show (Location n)
deriving instance Eq (Location n)

instance FromJSON (Location 'Unvalidated) where
  parseJSON (String t) = pure $ LocU t
  parseJSON u = typeMismatch "Expected Location U" u

instance ToJSON (Location 'Validated) where
  toJSON (LocV t) = String t            

instance Cql (Location 'Validated) where
  ctype = Tagged TextColumn
  toCql (LocV b) = CqlText b
  fromCql (CqlText b) = Right (LocV b)
  fromCql _           = Left "Location V: Expected CqlText"

-- no restrictions for now  
instance Validatable (Maybe (Location 'Unvalidated)) where   
  type Unvalidated (Maybe (Location 'Unvalidated)) = Maybe (Location 'Validated)
  validate (Just (LocU loc)) = success . return . LocV $ loc
  validate Nothing = success Nothing 

-------- Username ----------------------------------------         
data Username :: ValidationState -> * where 
  UsernameV :: Text -> Username 'Validated
  UsernameU :: Text -> Username 'Unvalidated

usernameT :: Username a -> Text
usernameT (UsernameV u) = u
usernameT (UsernameU u) = u
          
validateUsername :: Username 'Unvalidated -> Maybe (Username 'Validated)
validateUsername (T.strip . usernameT ->  n)                 
  | T.length n <= 15 && not (T.null n) && T.all isAlphaNum n =
    return $ UsernameV n
  | otherwise = Nothing              

deriving instance Show (Username n)
deriving instance Eq (Username n)         

instance FromJSON (Username 'Unvalidated) where
  parseJSON (String t) = pure $ UsernameU t
  parseJSON u = typeMismatch "Expected Username U" u

instance ToJSON (Username 'Validated) where
  toJSON (UsernameV t) = String t

instance ToJSON (Username 'Unvalidated) where
  toJSON (UsernameU t) = String t  

instance Cql (Username 'Validated) where
  ctype = Tagged TextColumn
  toCql (UsernameV u) = CqlText u
  fromCql (CqlText u) = Right (UsernameV u)
  fromCql _           = Left "Username V: Expected CqlText" 

instance Validatable (Username 'Unvalidated) where   
  type Unvalidated (Username 'Unvalidated) = Username 'Validated
  validate (uname) =
    maybe (failure $ return InvalidUsername)
          success
          (validateUsername uname)
    
-------- Name -----------------------------------------
data Name :: ValidationState -> * where 
  NameV :: Text -> Name 'Validated
  NameU :: Text -> Name 'Unvalidated

nameT :: Name a -> Text
nameT (NameV a) = a
nameT (NameU b) = b

validateName :: Name 'Unvalidated -> Maybe (Name 'Validated)
validateName (T.strip . nameT  -> n)
  | T.length n <= 20 && not (T.null n) = return $ NameV n
  | otherwise = Nothing         

deriving instance Show (Name n)
deriving instance Eq (Name n)             

instance FromJSON (Name 'Unvalidated) where
  parseJSON (String t) = pure $ NameU t
  parseJSON u = typeMismatch "Expected Name U" u

instance ToJSON (Name 'Validated) where
  toJSON (NameV t) = String t

instance Cql (Name 'Validated) where
  ctype = Tagged TextColumn
  toCql (NameV n) = CqlText n
  fromCql (CqlText n) = Right (NameV n)
  fromCql _           = Left "Name V: Expected CqlText"

instance Validatable (Name 'Unvalidated) where   
  type Unvalidated (Name 'Unvalidated) = Name 'Validated
  validate (name') =
    maybe (failure $ return InvalidName)
          success
          (validateName name')

-------- Bio -----------------------------------------
data Bio :: ValidationState -> * where 
  BioV :: Text -> Bio 'Validated
  BioU :: Text -> Bio 'Unvalidated

deriving instance Show (Bio b)
deriving instance Eq (Bio b)

instance FromJSON (Bio 'Unvalidated) where
  parseJSON (String t) = pure $ BioU t
  parseJSON u = typeMismatch "Expected Bio U" u

instance ToJSON (Bio 'Validated) where
  toJSON (BioV t) = String t

bioT :: Bio a -> Text
bioT (BioV a) = a
bioT (BioU a) = a   
         
validateBio :: Text -> Maybe (Bio 'Validated)
validateBio (T.strip -> b)
  | T.length b <= 160 = Just (BioV b)
  | otherwise = Nothing  

instance Cql (Bio 'Validated) where
  ctype = Tagged TextColumn
  toCql (BioV b) = CqlText b
  fromCql (CqlText b) = Right (BioV b)
  fromCql _           = Left "Bio V: Expected CqlText"    

instance Validatable (Maybe (Bio 'Unvalidated)) where   
  type Unvalidated (Maybe (Bio 'Unvalidated)) = Maybe (Bio 'Validated)
  validate (Nothing) = success Nothing
  validate (Just (BioU bio')) =
    maybe (failure $ return InvalidBio)
        (success . Just)
        (validateBio bio')              

--------Account---------------------------------------                   
data Account = Account
  { accUid            :: !UserId
  , accUsername       :: !(Username 'Validated)
  , accName           :: !(Name 'Validated)
  , accEmail          :: !(Email 'Validated)
  , accPassword       :: !(Password 'Encrypted)
  , accBio            :: !(Maybe (Bio 'Validated))
  , accUrl            :: !(Maybe (Url 'Validated))
  , accAvatarUrl      :: !(Maybe (Url 'Validated))
  , accLocation       :: !(Maybe (Location 'Validated))
  , accActivatedAt    :: !UTCTime
  , accSuspendededAt  :: !(Maybe UTCTime)
  , accCreatedAt      :: !UTCTime
  , accUpdatedAt      :: !(Maybe UTCTime)
  , accState          :: !AccountState
  } deriving (Show)  

instance ToJSON Account where
  toJSON (Account {..}) = object [
    "uid"  .= accUid,  "username" .= accUsername,
    "name" .= accName, "email"    .= accEmail,
    "bio"  .= accBio,  "url"      .= accUrl,
    "avatarUrl" .= accAvatarUrl, "createdAt" .= accCreatedAt,
    "location"  .= accLocation,  "updatedAt" .= accUpdatedAt ]
    
recordInstance ''Account

-- Docs               

instance ToSample Account where
  toSamples _ = singleSample $ Account 
    (UserId (fromJust (UUID.fromString "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf")))
    (UsernameV "onurz")
    (NameV "Onur")
    (fromJust (validateEmail (EmailU "foo@bar.com")))
    (encrypt' "12345678")
    (Just (BioV "some stuff"))
    (validateUrl "www.foo.bar.com")
    (validateUrl "aws/s3/avatar.png")
    (Just (LocV "San Jose, CA"))
    (UTCTime (ModifiedJulianDay 57670) 18000)
    Nothing
    (UTCTime (ModifiedJulianDay 57670) 18000)
    Nothing
    Active
  
---------- New Account ------------------     
data InitState 
  = UnInit -- ^ New account details verified but not persisted
  | Init -- ^ New account details verified and persisted

data NewAccount :: InitState -> * where 
  NewAccountU :: Username 'Validated
              -> Email 'Validated
              -> Password 'ClearText
              -> Name 'Validated
              -> Maybe (Bio 'Validated)
              -> Maybe WebUrl
              -> Maybe AvatarUrl
              -> Maybe (Location 'Validated)
              -> NewAccount 'UnInit 
  NewAccountI :: UserId
              -> Username 'Validated
              -> Email 'Validated
              -> Password 'Encrypted
              -> Name 'Validated
              -> Maybe (Bio 'Validated)
              -> Maybe WebUrl
              -> Maybe AvatarUrl
              -> Maybe (Location 'Validated)
              -> ActivatedAt 
              -> CreatedAt    
              -> NewAccount 'Init

---------- New Account Accessors -------------  
newAccUserId :: NewAccount 'Init -> UserId
newAccUserId (NewAccountI id' _ _ _ _ _ _ _ _ _ _) = id'

newAccUsername :: NewAccount a -> Username 'Validated
newAccUsername (NewAccountI _ u _ _ _ _ _ _ _ _ _) = u
newAccUsername (NewAccountU e _ _ _ _ _ _ _ ) = e
            
newAccEmail :: NewAccount a -> Email 'Validated
newAccEmail (NewAccountI _ _ e _ _ _ _ _ _ _ _) = e  
newAccEmail (NewAccountU _ e _ _ _ _ _ _ ) = e           
               
newAccPassword :: NewAccount 'Init -> Password 'Encrypted
newAccPassword (NewAccountI _ _ _ p _ _ _ _ _ _ _) = p

newAccPassword' :: NewAccount 'UnInit -> Password 'ClearText
newAccPassword' (NewAccountU _ _ c _ _ _ _ _) = c            

newAccName :: NewAccount a -> Name 'Validated
newAccName (NewAccountI _ _ _ _ n _ _ _ _ _ _) = n
newAccName (NewAccountU _ _ _ n _ _ _ _ ) = n
          
newAccBio :: NewAccount a -> Maybe (Bio 'Validated)
newAccBio (NewAccountI _ _ _ _ _ b _ _ _ _ _) = b
newAccBio (NewAccountU _ _ _ _ b _ _ _ ) = b          

newAccWebUrl :: NewAccount a -> Maybe WebUrl
newAccWebUrl (NewAccountI _ _ _ _ _ _ w _ _ _ _) = w
newAccWebUrl (NewAccountU _ _ _ _ _ w _ _ ) = w              

newAccAvatarUrl :: NewAccount a -> Maybe AvatarUrl
newAccAvatarUrl (NewAccountI _ _ _ _ _ _ _ a _ _ _) = a              
newAccAvatarUrl (NewAccountU _ _ _ _ _ _ a _ ) = a
             
newAccLocation :: NewAccount a -> Maybe (Location 'Validated)
newAccLocation (NewAccountI _ _ _ _ _ _ _ _ l _ _) = l
newAccLocation (NewAccountU _ _ _ _ _ _ _ l) = l               

newAccActivatedAt :: NewAccount 'Init -> ActivatedAt
newAccActivatedAt (NewAccountI _ _ _ _ _ _ _ _ _ a _) = a

newAccCreatedAt :: NewAccount 'Init -> CreatedAt
newAccCreatedAt (NewAccountI _ _ _ _ _ _ _ _ _ _ c) = c               

type AccCreationTuple =
    (UserId, Username 'Validated, Name 'Validated, Email 'Validated,
     Password 'Encrypted, Maybe (Bio 'Validated), Maybe WebUrl, Maybe AvatarUrl,
     Maybe (Location 'Validated), ActivatedAt, CreatedAt, AccountState)
                
newAccountUnInit :: Username 'Unvalidated
                 -> Email 'Unvalidated
                 -> Password 'PWUnvalidated
                 -> Name 'Unvalidated
                 -> Maybe (Bio 'Unvalidated)
                 -> Maybe (Url 'Unvalidated)
                 -> Maybe (Url 'Unvalidated)
                 -> Maybe (Location 'Unvalidated)
                 -> OpValidation (NewAccount 'UnInit)
newAccountUnInit uname email pass name' mbio url' avaUrl loc = 
  NewAccountU
    <$> validate uname
    <*> validate email
    <*> validate pass
    <*> validate name'
    <*> validate mbio
    <*> validate url'
    <*> validate avaUrl
    <*> validate loc

instance FromJSON (OpValidation (NewAccount 'UnInit)) where
  parseJSON (Object o) =       
    newAccountUnInit
      <$> o .: "username"
      <*> o .: "email"
      <*> o .: "password"
      <*> o .: "name"
      <*> o .:? "bio"
      <*> o .:? "url"
      <*> o .:? "avatarUrl"
      <*> o .:? "location"
  parseJSON unknown = typeMismatch "NewAccount" unknown
    
instance ToJSON (NewAccount 'Init) where
  toJSON (NewAccountI id' u e _ n b url' avaUrl loc a c) = object
    [ "id" .= id', "username" .= u, "email" .= e
    , "name" .= n, "bio" .= b, "url" .= url', "avatarUrl" .= avaUrl
    , "location" .= loc, "activatedAt" .= a, "createdAt" .= c
    ]
             
instance Show (NewAccount a) where
  show (NewAccountU u e _ n b url' avaUrl loc) = "NewAccountU {" ++
    "username = " ++ show u   ++ ", " ++ "name =" ++ show n ++ ", " ++
    "email = "  ++ show e   ++ ", " ++ "bio = " ++ show b ++ ", " ++
    "webUrl = " ++ show url' ++ ", " ++ "avatarUrl = " ++ show avaUrl ++ ", " ++
    "location = " ++ show loc ++ "}" 
  show (NewAccountI id' u e _ n b url' avaUrl loc a c) = "NewAccountI {" ++
    "id = "    ++ show id' ++ ", " ++ "username = " ++ show u ++ ", " ++
    "email = " ++ show e ++  ", " ++ "name ="      ++ show n ++ ", " ++
    "webUrl = " ++ show url' ++ ", " ++ "avatarUrl = " ++ show avaUrl ++ ", " ++
    "bio = "   ++ show b ++  ", " ++ "location = " ++ show loc ++ ", " ++
    "activatedAt = " ++ show a ++ ", " ++ "createdAt =" ++ show c ++ ", " ++ "}"

-- Docs  
  
instance ToSample (OpValidation (NewAccount 'UnInit)) where
  toSamples _ = singleSample $
    newAccountUnInit
      (UsernameU "onurz")
      (EmailU "foo@bar.com")
      (UnValidatedPass "12345678")
      (NameU "Onur")
      (Just (BioU "some stuff"))
      (Just (UrlU "www.foo.bar.com"))
      (Just (UrlU "aws/s3/avatar.png"))
      (Just (LocU "San Jose, CA"))

instance ToSample (NewAccount 'Init) where
  toSamples _ = singleSample $ NewAccountI 
    (UserId (fromJust (UUID.fromString "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf")))
    (UsernameV "onurz")
    (fromJust (validateEmail (EmailU "foo@bar.com")))
    (encrypt' "12345678")
    (NameV "Onur")
    (Just (BioV "some stuff"))
    (validateUrl "www.foo.bar.com")
    (validateUrl "aws/s3/avatar.png")
    (Just (LocV "San Jose, CA"))
    (UTCTime (ModifiedJulianDay 57670) 18000)
    (UTCTime (ModifiedJulianDay 57670) 18000)

instance ToJSON (OpValidation (NewAccount 'UnInit)) where
  toJSON (Success (NewAccountU u e _ n b url' avaUrl loc)) = object 
    [ "username"   .= u
    , "email"      .= e
    , "name"       .= n
    , "bio"        .= b
    , "webUrl"     .= url'
    , "avatarUrl"  .= avaUrl
    , "location"   .= loc
    , "password"   .= T.pack ""
    ]
  toJSON (Failure e) =
    error $ "Failed converting 'NewAccount 'UnInit: " ++ show e

--------Profile---------------------------------------
data Profile = Profile
  { pfUsername    :: !(Username 'Validated)
  , pfEmail       :: !(Email 'Validated)
  , pfName        :: !(Name 'Validated)
  , pfBio         :: !(Maybe (Bio 'Validated))
  , pfWebUrl      :: !(Maybe WebUrl)
  , pfAvatarUrl   :: !(Maybe AvatarUrl)
  , pfLocation    :: !(Maybe (Location 'Validated))
  } deriving (Show)
  
        
profileV :: Username 'Unvalidated
         -> Email 'Unvalidated
         -> Name 'Unvalidated
         -> Maybe (Bio 'Unvalidated)
         -> Maybe (Url 'Unvalidated)
         -> Maybe (Url 'Unvalidated)
         -> Maybe (Location 'Unvalidated)
         -> OpValidation Profile
profileV uname email name' mbio mwebUrl mavatarUrl mloc =
  Profile             
    <$> validate uname
    <*> validate email
    <*> validate name'
    <*> validate mbio
    <*> validate mwebUrl
    <*> validate mavatarUrl
    <*> validate mloc
   
instance FromJSON (OpValidation Profile) where
  parseJSON (Object o) = profileV
    <$> o .:  "username"
    <*> o .:  "email"
    <*> o .:  "name"
    <*> o .:? "bio"
    <*> o .:? "webUrl"
    <*> o .:? "avatarUrl"
    <*> o .:? "location"
  parseJSON unknown = typeMismatch "Profile" unknown

deriveToJSON (prefixRemovedOpts 2) ''Profile      

-- Docs             
             
instance ToJSON (OpValidation Profile) where
  toJSON (Success p) = toJSON p
  toJSON (Failure e) =
    error $ "Failed converting Profile to JSON: " ++ show e

instance ToSample Profile where
  toSamples _ = singleSample $
    Profile
      (UsernameV "onurz")
      (fromJust (validateEmail (EmailU "foo@bar.com")))  
      (NameV "Onur")
      (Just (BioV "some stuff"))
      (validateUrl "www.foo.bar.com")
      (validateUrl "www.aws.com/s3/avatar.png")
      (Just (LocV "San Jose, CA"))
  
instance ToSample (OpValidation Profile) where
  toSamples _ = singleSample $
    profileV
      (UsernameU "onurz")
      (EmailU "foo@bar.com")
      (NameU "Onur")
      (Just (BioU "some stuff"))
      (Just (UrlU "www.foo.bar.com"))
      (Just (UrlU "aws/s3/avatar.png"))
      (Just (LocU "San Jose, CA"))  

----------------- DB OPs ------------------------------
updateProfile :: (CassClient m, TimeGetter m)
              => UserId
              -> Profile
              -> m (Either [UserOpFailure] Profile)
updateProfile uid prof@Profile{..} = do
  macc <- getUserAccById uid
  case macc of
    Nothing -> return $ Left [UserNotFound]
    Just (Account{..}) -> do
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
                  -> Email 'Validated
                  -> Username 'Validated
                  -> m ()
   updateProfile' updateOp ce cu email uname =
     runCassOp $ CQ.batch $
       execState (batchS ce cu email uname)
         (CQ.setType BatchLogged >> updateOp)
    where
      batchS :: Bool
             -> Bool
             -> Email 'Validated
             -> Username 'Validated
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
              -> Email 'Validated            
              -> Username 'Validated
              -> State (ExceptT UserOpFailure m ()) ()
       update changeE changeU newE newU = do
         when changeE $ modify                    
           (>> insertEmail newE uid)
         when changeU $ modify
           (>> insertUsername newU uid)
      
   qUpdateAcc :: CQ.PrepQuery W (Username 'Validated, Name 'Validated, 
       Email 'Validated, Maybe (Bio 'Validated), Maybe WebUrl, Maybe AvatarUrl,
       Maybe (Location 'Validated), UpdatedAt, UserId) ()
   qUpdateAcc = CQ.prepared
     "update user_account set user_name =?, name =?,\ 
      \ email =?, bio =?, url =?, avatar_url =?, location =?, \ 
      \ updated_at = ? where id =?"

   qDelEmail :: CQ.PrepQuery W (Identity (Email 'Validated)) ()
   qDelEmail = CQ.prepared
     "delete from login_by_email where email = ?"

   qDelUsername :: CQ.PrepQuery W (Identity (Username 'Validated)) ()
   qDelUsername = CQ.prepared
     "delete from login_by_user_name where user_name = ?"
                 
deleteUsername :: CassClient m => Username 'Validated -> m ()   
deleteUsername uname = runCassOp $ CQ.write q p        
 where
   q :: CQ.PrepQuery W (Identity (Username 'Validated)) ()
   q = CQ.prepared "delete from login_by_user_name where user_name = ?"

   p :: QueryParams (Identity (Username 'Validated)) 
   p = defQueryParams (Identity uname)
  
deleteEmail :: CassClient m => Email 'Validated -> m ()
deleteEmail email = runCassOp $ CQ.write q p 
 where
   q :: CQ.PrepQuery W (Identity (Email 'Validated)) ()
   q = CQ.prepared "delete from login_by_email where email = ?"

   p :: QueryParams (Identity (Email 'Validated))
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

insertUsername :: Username 'Validated -> UserId -> CQErr UserOpFailure ()
insertUsername uname uid = do
  [transRes] <- lift $ runCassOp $ CQ.trans q p
  unless (rowLength transRes == 1) $ throwE UsernameExists  
  
 where
   q :: CQ.PrepQuery W (Username 'Validated, UserId) Row
   q = CQ.prepared "insert into login_by_user_name(user_name, user_id) \
                      \ values(?, ?) if not exists"
        
   p :: QueryParams (Username 'Validated, UserId) 
   p = defQueryParamsMeta (uname, uid)

insertEmail :: Email 'Validated -> UserId -> CQErr UserOpFailure ()
insertEmail e uid = do
  [transRes] <- lift $ runCassOp $ CQ.trans q p
  unless (rowLength transRes == 1) $ throwE EmailExists  
 where
   q :: CQ.PrepQuery W (Email 'Validated, UserId) Row
   q = CQ.prepared "insert into login_by_email(email, user_id) \
                     \ values(?, ?) if not exists"
         
   p :: QueryParams (Email 'Validated, UserId) 
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
                 => Email 'Validated
                 -> m (Maybe UserId)
getUserIdByEmail email = fmap runIdentity <$> runCassOp (CQ.query1 q p)
 where
   q :: CQ.PrepQuery R (Identity (Email 'Validated)) (Identity UserId)
   q = CQ.prepared "select user_id from login_by_email where email = ?"

   p :: QueryParams (Identity (Email 'Validated))
   p = defQueryParams (Identity email)

getUserIdByUsername :: CassClient m
                    => Username 'Validated
                    -> m (Maybe UserId) 
getUserIdByUsername uname = fmap runIdentity <$> runCassOp (CQ.query1 q p)
 where
   q :: CQ.PrepQuery R (Identity (Username 'Validated)) (Identity UserId)
   q = CQ.prepared "select user_id from login_by_user_name where user_name = ?"

   p :: QueryParams (Identity (Username 'Validated))
   p =  defQueryParams (Identity uname)

newAccount :: (CassClient m, TimeGetter m, UUIDGenerator m, PasswordEncryptor m)
           => NewAccount 'UnInit
           -> m (Either [UserOpFailure] (NewAccount 'Init))
newAccount (NewAccountU uname email cp name' mbio murl mAvaUrl mloc) = do
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
     let acc = NewAccountI uid uname email ep name'
                 mbio murl mAvaUrl mloc curT curT
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
