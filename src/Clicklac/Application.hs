{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Clicklac.Application
  (
  -- * App Monad
    App
  , runApp
  
  -- * Data types
  , AppConfig(..)  
  ) where

import Data.Composition ((.**))                       
import qualified Data.Text as T
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Except (ExceptT)      
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO(..)) 
import Control.Monad.Reader.Class
  ( asks
  , MonadReader
  , local
  ) 
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..)) 
import Control.Monad.Error.Class (MonadError(..))
       
import Crypto.Nonce (Generator)
import Crypto.PasswordStore (makePasswordWith)
import Database.PostgreSQL.Simple.Lifted (PostgresClient(..))
import Database.PostgreSQL.Simple.Pool
  ( PostgresPool
  , withConnPool
  )       
import qualified Data.Vault.Lazy as VT
import qualified Data.Time.Clock as Clock (getCurrentTime)
import qualified Data.Time.Clock.POSIX as POSIX (getPOSIXTime)
import qualified Data.UUID.V1 as UUIDv1       
import qualified Data.UUID.V4 as UUIDv4
import qualified Crypto.Nonce as N
import qualified Network.HostName as NH (getHostName)       
import System.Logger (Logger)
import System.Logger.Class (MonadLogger(..))              
import qualified System.Logger as L             
import Database.CQL.IO (ClientState, MonadClient(..), runClient)       
import Servant.Server.Internal.ServantErr (ServantErr)  
import qualified Web.ClientSession as WCS
       
import Clicklac.Types
  ( UserId
  , CookieEncryption(..)
  , NonceGenerator(..)
  , UUIDGenerator(..)
  , HostnameGetter(..)
  , CassClient(..)
  , TimeGetter(..)
  , PasswordEncryptor(..)
  )                 

data AppConfig = AppConfig
  { csState       :: ClientState
  , logger        :: Logger
  , nonceGen      :: Generator -- ^ used to generate hard-to-predict session IDs
  , clientSessKey :: WCS.Key -- ^ cookie encryption key
  , vaultKey      :: VT.Key UserId
  , psgConnPool   :: PostgresPool
  }        
  
newtype App a = A {
    runA :: ReaderT AppConfig (ExceptT ServantErr IO) a
  } deriving (Monad, Functor, Applicative, MonadIO,
              MonadReader AppConfig, MonadError ServantErr,
              MonadCatch, MonadThrow, MonadBase IO)
  
-- Deriving MonadMask is not possible here because the base monad is ExceptT.
  
instance MonadBaseControl IO App where
-- type StM App a = StM (ReaderT AppConfig (ExceptT ServantErr IO)) a
  type StM App a = Either ServantErr a
  liftBaseWith f = A $ liftBaseWith $ \q -> f (q . runA)
  restoreM = A . restoreM         
  
instance CookieEncryption App where
 encryptCookie content = do
   key <- asks clientSessKey
   liftIO . WCS.encryptIO key $ content
 decryptCookie dataBS64 = do
   key <- asks clientSessKey
   return $ WCS.decrypt key dataBS64

instance NonceGenerator App where
  generateNonce = N.nonce128urlT =<< asks nonceGen

instance UUIDGenerator App where
  generateUUIDv1 = liftIO UUIDv1.nextUUID                    
  generateUUIDv4 = liftIO UUIDv4.nextRandom           

instance HostnameGetter App where
  getHostname = T.pack `fmap` liftIO NH.getHostName

instance CassClient App where
  runCassOp cl = flip runClient cl =<< asks csState  

instance TimeGetter App where
  getPosixTime = liftIO POSIX.getPOSIXTime 
  getCurrentTime = liftIO Clock.getCurrentTime  

instance PasswordEncryptor App where
  encryptPassword = liftIO .** makePasswordWith         

instance PostgresClient App where
  liftPSGClient f = flip withConnPool f =<< asks psgConnPool           

instance MonadClient App where
  liftClient a = flip runClient a =<< asks csState
  localState f app = do
    flip local app $
      \conf -> conf {csState = f $ csState conf} 

instance MonadLogger App where
  log l m = asks logger >>= \g -> L.log g l m
    
runApp :: App a -> AppConfig -> ExceptT ServantErr IO a
runApp a c = runReaderT (runA a) c
