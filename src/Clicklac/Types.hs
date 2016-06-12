
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE RankNTypes                 #-}

module Clicklac.Types
  (
  -- * Type classes
    CookieEncryption(..)
  , NonceGenerator(..)
  , UUIDGenerator(..)
  , HostnameGetter(..)
  , CassClient(..)
  , TimeGetter(..)

  -- * Type synonyms
  , CQErr

  -- * Data types
  , module X  
  ) where

import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)

import qualified Database.CQL.IO as CS
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)

import Clicklac.Types.Bio as X
import Clicklac.Types.Email as X
import Clicklac.Types.Location as X
import Clicklac.Types.Name as X
import Clicklac.Types.Password as X
import Clicklac.Types.UserId as X
import Clicklac.Types.Username as X
import Clicklac.Types.Url as X
import Clicklac.Types.ValidationState as X

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
  
