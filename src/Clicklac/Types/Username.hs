
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE ViewPatterns       #-}

module Clicklac.Types.Username
  ( usernameT
  , Username
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

newtype Username a = Username Text

usernameT :: Username a -> Text
usernameT (Username u) = u

validateUsername :: Text -> Maybe (Username Validated)
validateUsername (T.strip ->  n)
  | T.length n <= 15 && not (T.null n) && T.all isAlphaNum n =
    return $ Username n
  | otherwise = Nothing

deriving instance Show (Username n)
deriving instance Eq (Username n)

instance FromJSON (Username Unvalidated) where
  parseJSON (String t) = pure $ Username t
  parseJSON u = typeMismatch "Expected Username " u

instance ToJSON (Username Validated) where
  toJSON (Username t) = String t

instance Cql (Username Validated) where
  ctype = Tagged TextColumn
  toCql (Username u) = CqlText u
  fromCql (CqlText u) = Right (Username u)
  fromCql _           = Left "Username: Expected CqlText"

instance Validatable (Username Unvalidated) where
  type Unvalidated' (Username Unvalidated) = Username Validated
  validate (Username uname) =
    maybe (failure $ return InvalidUsername)
          success
          (validateUsername uname)
