
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module Clicklac.Types.Email
  ( Email
  , getEmailT
  , validateEmail
  ) where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)

import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import Data.Aeson.Types (Value(String), typeMismatch)
import Database.CQL.Protocol
  ( Cql(..)
  , Tagged(..)
  , ColumnType (TextColumn)
  , Value(CqlText)
  )
import Text.Email.Validate (canonicalizeEmail)

import Clicklac.Types.ValidationState
import Clicklac.Validation
  ( Validatable (..)
  , ValidationFailure (InvalidEmail)
  , success
  , failure
  )

newtype Email a = Email Text

getEmailT :: Email a -> Text
getEmailT (Email e) = e

validateEmail :: Text -> Maybe (Email Validated)
validateEmail email =
  case canonicalizeEmail $ TE.encodeUtf8 email of
    (Just e) -> return . Email . TE.decodeUtf8 $ e
    _ -> Nothing

deriving instance Eq (Email a)
deriving instance Show (Email a)

instance ToJSON (Email Validated) where
  toJSON (Email e) = String e

instance FromJSON (Email Unvalidated) where
  parseJSON (String v) = pure $ Email v
  parseJSON unknown = typeMismatch "Email" unknown

instance Cql (Email Validated) where
  ctype = Tagged TextColumn
  toCql (Email e) = CqlText e
  fromCql (CqlText e) = Right (Email e)
  fromCql _           = Left "Expected CqlText"

instance Validatable (Email Unvalidated) where
  type Unvalidated' (Email Unvalidated) = Email Validated
  validate (Email email) =
    maybe (failure $ return InvalidEmail)
          success
          (validateEmail email)
