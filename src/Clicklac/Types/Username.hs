
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module Clicklac.Types.Username
  ( usernameT
  , Username (UsernameU)
  , validateUsername
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
import Data.Char (isAlphaNum)
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
  , ValidationFailure (InvalidUsername)
  , failure
  , success
  )

data Username :: ValidationState -> * where
  UsernameV :: Text -> Username 'Validated
  UsernameU :: Text -> Username 'Unvalidated

usernameT :: Username a -> Text
usernameT (UsernameV u) = u
usernameT (UsernameU u) = u

validateUsername :: Username 'Unvalidated -> Maybe (Username 'Validated)
validateUsername (T.strip . usernameT ->  n)
  | T.length n <= 15 && not (T.null n) && T.all isAlphaNum n =
    return $ UsernameV n
  | otherwise = Nothing

deriving instance Show (Username n)
deriving instance Eq (Username n)

instance FromJSON (Username 'Unvalidated) where
  parseJSON (String t) = pure $ UsernameU t
  parseJSON u = typeMismatch "Expected Username U" u

instance ToJSON (Username 'Validated) where
  toJSON (UsernameV t) = String t

instance ToJSON (Username 'Unvalidated) where
  toJSON (UsernameU t) = String t

instance Cql (Username 'Validated) where
  ctype = Tagged TextColumn
  toCql (UsernameV u) = CqlText u
  fromCql (CqlText u) = Right (UsernameV u)
  fromCql _           = Left "Username V: Expected CqlText"

instance Validatable (Username 'Unvalidated) where
  type Unvalidated (Username 'Unvalidated) = Username 'Validated
  validate uname =
    maybe (failure $ return InvalidUsername)
          success
          (validateUsername uname)
