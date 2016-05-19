
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module Clicklac.Types.Bio
  ( bioT
  , validateBio
  , Bio(BioU)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

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
  , ValidationFailure (InvalidBio)
  , failure
  , success
  )

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
  validate Nothing = success Nothing
  validate (Just (BioU bio')) =
    maybe (failure $ return InvalidBio)
        (success . Just)
        (validateBio bio')
