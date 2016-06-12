
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module Clicklac.Types.Name
  ( nameT
  , Name
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

newtype Name a = Name Text

nameT :: Name a -> Text
nameT (Name a) = a

validateName :: Text -> Maybe (Name Validated)
validateName (T.strip -> n)
  | T.length n <= 20 && not (T.null n) = return $ Name n
  | otherwise = Nothing

deriving instance Show (Name n)
deriving instance Eq (Name n)

instance FromJSON (Name Unvalidated) where
  parseJSON (String t) = pure $ Name t
  parseJSON u = typeMismatch "Expected Name" u

instance ToJSON (Name Validated) where
  toJSON (Name t) = String t

instance Cql (Name Validated) where
  ctype = Tagged TextColumn
  toCql (Name n) = CqlText n
  fromCql (CqlText n) = Right (Name n)
  fromCql _           = Left "Name: Expected CqlText"

instance Validatable (Name Unvalidated) where
  type Unvalidated' (Name Unvalidated) = Name Validated
  validate (Name name') =
    maybe (failure $ return InvalidName)
          success
          (validateName name')
