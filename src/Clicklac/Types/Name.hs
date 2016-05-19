
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module Clicklac.Types.Name
  ( nameT
  , Name(NameU)
  , validateName
  ) where

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
import Data.Text (Text)
import qualified Data.Text as T
import Database.CQL.Protocol
  ( ColumnType (TextColumn)
  , Cql (..)
  , Tagged (..)
  , Value (CqlText)
  )

import Clicklac.Types.ValidationState
import Clicklac.Validation
  ( Validatable (..)
  , ValidationFailure (InvalidName)
  , failure
  , success
  )

data Name :: ValidationState -> * where
  NameV :: Text -> Name 'Validated
  NameU :: Text -> Name 'Unvalidated

nameT :: Name a -> Text
nameT (NameV a) = a
nameT (NameU b) = b

validateName :: Name 'Unvalidated -> Maybe (Name 'Validated)
validateName (T.strip . nameT -> n)
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
  validate name' =
    maybe (failure $ return InvalidName)
          success
          (validateName name')
