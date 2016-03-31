{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}    
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}    
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
   
module Clicklac.Types
  (
  -- * UserId 
    UserId(..)
  , generateUserId
  , userIdUUID

  -- * Type classes
  , ValidationState(..)
  , CookieEncryption(..)
  , NonceGenerator(..)
  , UUIDGenerator(..)
  , HostnameGetter(..)
  , CassClient(..)
  , TimeGetter(..)
  , PasswordEncryptor(..)

  -- * Type synonyms
  , CQErr
  ) where

import Control.Monad (mzero, liftM)
import Data.ByteString (ByteString)
import Data.Text (Text) 
import qualified Data.Text as T      
import Data.UUID (UUID)
import qualified Data.UUID as UUID       
import Control.Monad.Trans.Except (ExceptT)
       
import Crypto.PasswordStore (Salt)       
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON) 
import Data.Aeson.Types (Value(..), typeMismatch)
import qualified Database.CQL.IO as CS       
import Database.CQL.Protocol
  ( Cql(..)
  , Tagged(..)
  , ColumnType (UuidColumn)
  , Value (CqlUuid)
  )  
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Data.Serialize (Serialize)       
import qualified Data.Serialize as Ser       
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Servant (Capture, FromHttpApiData(..))       
import Servant.Docs (ToCapture(..), DocCapture(..))     
       
data ValidationState
   = Unvalidated
   | Validated

------------ Type Classes ------------------------------  
class Monad m => CookieEncryption m where
  encryptCookie :: ByteString -> m ByteString 
  decryptCookie :: ByteString -> m (Maybe ByteString)

class Monad m => NonceGenerator m where
  generateNonce :: m Text

class Monad m => UUIDGenerator m where
  generateUUIDv1 :: m (Maybe UUID)      
  generateUUIDv4 :: m UUID

class Monad m => HostnameGetter m where
  getHostname :: m Text
 
class (Monad m) => CassClient m where
  runCassOp :: CS.Client a -> m a
  
type CQErr e a = forall m. CassClient m => ExceptT e m a

class (Monad m) => TimeGetter m where
  getPosixTime   :: m POSIXTime -- seconds since 1970
  getCurrentTime :: m UTCTime

class (Monad m) => PasswordEncryptor m where
  encryptPassword :: (ByteString -> Salt -> Int -> ByteString)
                  -> ByteString
                  -> Int
                  -> m ByteString     

-------------- UserId ---------------------   
newtype UserId = UserId UUID
  deriving (Show, Eq, FromField)

userIdUUID :: UserId -> UUID 
userIdUUID (UserId uuid') = uuid'           
  
generateUserId :: (UUIDGenerator m) => m UserId
generateUserId = liftM UserId generateUUIDv4  

instance FromHttpApiData UserId where         
  parseUrlPiece t =
    case UUID.fromText t of
      Just uuid -> Right $ UserId uuid
      Nothing -> Left $ T.pack "Failed to convert to UUID"
  
instance ToCapture (Capture "userid" UserId) where
  toCapture _ =
    DocCapture "userid" 
               "(UUID) user id"  
  
instance ToField UserId where
  toField (UserId id') = toField id'           

instance ToJSON UserId where
  toJSON (UserId id') = String $ UUID.toText id'  

instance FromJSON UserId where
  parseJSON (String u) = maybe mzero (pure . UserId) (UUID.fromText u) 
  parseJSON u = typeMismatch "UserId" u
  
instance Cql UserId where
  ctype = Tagged UuidColumn
  toCql   (UserId u) = CqlUuid u
  fromCql (CqlUuid u) = Right (UserId u)
  fromCql _           = Left "Expected CqlText"

instance Serialize UserId where
  put (UserId id')  = Ser.put . UUID.toByteString $ id'
  get = maybe (error "Not a valid UUID") (return . UserId) =<<
          fmap UUID.fromByteString Ser.get             
