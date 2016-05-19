
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Clicklac.Types.Location
  ( locationT
  , Location (LocU)
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
