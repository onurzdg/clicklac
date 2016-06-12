
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Clicklac.Types.Location
  ( locationT
  , Location
  ) where

import Data.Text (Text)

import Data.Aeson
  ( FromJSON
  , ToJSON
  , parseJSON
  , toJSON
  )
import Data.Aeson.Types
  ( Value (..)
  , typeMismatch
  )
import Database.CQL.Protocol
  ( ColumnType (TextColumn)
  , Cql (..)
  , Tagged (..)
  , Value (CqlText)
  )

import Clicklac.Types.ValidationState
import Clicklac.Validation
  ( Validatable (..)
  , success
  )

newtype Location a = Location Text  

locationT :: Location a -> Text
locationT (Location l) = l

deriving instance Show (Location n)
deriving instance Eq (Location n)

instance FromJSON (Location Unvalidated) where
  parseJSON (String t) = pure $ Location t
  parseJSON u = typeMismatch "Expected Location U" u

instance ToJSON (Location Validated) where
  toJSON (Location t) = String t

instance Cql (Location Validated) where
  ctype = Tagged TextColumn
  toCql (Location b) = CqlText b
  fromCql (CqlText b) = Right (Location b)
  fromCql _           = Left "Location V: Expected CqlText"

-- no restrictions for now
instance Validatable (Maybe (Location Unvalidated)) where
  type Unvalidated' (Maybe (Location Unvalidated)) = Maybe (Location Validated)
  validate (Just (Location a)) = success . return $  Location a
  validate Nothing = success Nothing
