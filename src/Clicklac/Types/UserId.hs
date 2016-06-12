
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Clicklac.Types.UserId where

import Control.Monad (mzero)
import qualified Data.Text as T

import Data.Aeson.Types
  ( Value (..)
  , typeMismatch
  )
import Data.Aeson
  ( FromJSON
  , ToJSON
  , parseJSON
  , toJSON
  )
import Database.CQL.Protocol
  ( ColumnType (UuidColumn)
  , Cql (..)
  , Tagged (..)
  , Value (CqlUuid)
  )

import Data.Serialize (Serialize)
import qualified Data.Serialize as Ser
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField(..))
import Servant (FromHttpApiData(..))

import Data.UUID (UUID)
import qualified Data.UUID as UUID                 

newtype UserId = UserId UUID
  deriving (Show, Eq, FromField)

userIdUUID :: UserId -> UUID
userIdUUID (UserId uuid') = uuid'

instance FromHttpApiData UserId where
  parseUrlPiece t =
    case UUID.fromText t of
      Just uuid -> Right $ UserId uuid
      Nothing -> Left $ T.pack "Failed to convert userid to UUID"

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
