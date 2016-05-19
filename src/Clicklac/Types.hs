{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

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
  , module Clicklac.Types.Location
  , module Clicklac.Types.UserId
  , module Clicklac.Types.Name
  , module Clicklac.Types.Username
  , module Clicklac.Types.Bio
  , module Clicklac.Types.Email
  , module Clicklac.Types.Password
  , module Clicklac.Types.Url
  , module Clicklac.Types.ValidationState
  ) where

import Control.Monad.Trans.Except (ExceptT)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.UUID (UUID)

import qualified Database.CQL.IO as CS
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime)

import Clicklac.Types.Bio
import Clicklac.Types.Email
import Clicklac.Types.Location
import Clicklac.Types.Name
import Clicklac.Types.Password
import Clicklac.Types.UserId
import Clicklac.Types.Username
import Clicklac.Types.Url
import Clicklac.Types.ValidationState

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



