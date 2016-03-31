{-# LANGUAGE DataKinds             #-}    
{-# LANGUAGE FlexibleInstances     #-}    
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}   
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
    
module Clicklac.Email
  ( Email(EmailU)
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

import Clicklac.Validation
  ( Validatable (..)
  , ValidationFailure (InvalidEmail)
  , success
  , failure
  )    
import Clicklac.Types (ValidationState(..))              

data Email :: ValidationState -> * where
  EmailV :: Text -> Email 'Validated
  EmailU :: Text -> Email 'Unvalidated

getEmailT :: Email a -> Text
getEmailT (EmailV e) = e
getEmailT (EmailU e) = e

validateEmail :: Email 'Unvalidated -> Maybe (Email 'Validated)
validateEmail (EmailU email) =
  case canonicalizeEmail $ TE.encodeUtf8 email of            
    (Just e) -> return . EmailV . TE.decodeUtf8 $ e                         
    _ -> Nothing
  
deriving instance Eq (Email a)
deriving instance Show (Email a)
                 
instance ToJSON (Email 'Validated) where
  toJSON (EmailV e) = String e
 
instance FromJSON (Email 'Unvalidated) where
  parseJSON (String v) = pure $ EmailU v
  parseJSON unknown = typeMismatch "Email" unknown

instance ToJSON (Email 'Unvalidated) where
  toJSON (EmailU e) = String e         

instance Cql (Email 'Validated) where
  ctype = Tagged TextColumn
  toCql (EmailV e) = CqlText e
  fromCql (CqlText e) = Right (EmailV e)
  fromCql _           = Left "Expected CqlText"              

instance Validatable (Email 'Unvalidated) where   
  type Unvalidated (Email 'Unvalidated) = Email 'Validated
  validate email =  
    maybe (failure $ return InvalidEmail)
          success
          (validateEmail email)
