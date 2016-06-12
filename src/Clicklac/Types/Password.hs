
{-# LANGUAGE FlexibleInstances  #-}            
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}    
    
module Clicklac.Types.Password
  ( PasswordEncryptor(..)
  , EncryptedPass
  , ClearTextPass
  , validatePass
  , getClearText
  , toEncrypted
  , verify
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T (length)       
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
      
import Database.CQL.Protocol
  ( Cql(..)
  , Tagged(..)
  , ColumnType (TextColumn)
  , Value(CqlText)
  )
import Crypto.PasswordStore
  ( verifyPasswordWith
  , pbkdf2
  , Salt
  )
import Data.Aeson (FromJSON(..))
import Data.Aeson.Types (Value(String), typeMismatch)

import Clicklac.Types.ValidationState
import Clicklac.Validation
  ( Validatable(..)
  , ValidationFailure(PasswordLong, PasswordShort)
  , failure
  , success
  , OpValidation
  )           
      
newtype EncryptedPass = EncryptedPass Text
newtype ClearTextPass a = ClearTextPass Text 

instance Show EncryptedPass where
  show _ = ""

instance Show (ClearTextPass a) where
  show _ = ""  
                          
instance Eq (ClearTextPass Validated) where
  (ClearTextPass cp1) == (ClearTextPass cp2) = cp1 == cp2

instance FromJSON (ClearTextPass Unvalidated) where
  parseJSON (String p) = pure $ ClearTextPass p   
  parseJSON unknown = typeMismatch "ClearTextPass" unknown
  
instance Cql EncryptedPass where
  ctype = Tagged TextColumn
  toCql (EncryptedPass p) = CqlText p
  fromCql (CqlText p) = Right (EncryptedPass p)
  fromCql _           = Left "Expected CqlText"

class (Monad m) => PasswordEncryptor m where
  encryptPassword :: (ByteString -> Salt -> Int -> ByteString)
                  -> ByteString
                  -> Int
                  -> m ByteString  

defaultStrength :: Int
defaultStrength = 15
                
toEncrypted :: (PasswordEncryptor m)
            => ClearTextPass Validated
            -> m EncryptedPass
toEncrypted (ClearTextPass p) =
  (EncryptedPass . TE.decodeUtf8) `fmap`
    encrypt (TE.encodeUtf8 p) defaultStrength

getClearText :: ClearTextPass a -> Text   
getClearText (ClearTextPass p) = p    

validatePass :: Text 
             -> Either ValidationFailure (ClearTextPass Validated)
validatePass pass
  | pwLength > 100 = Left PasswordLong
  | pwLength < 6 = Left PasswordShort
  | otherwise = Right (ClearTextPass pass)
 where
   pwLength = T.length pass   
 
encrypt :: PasswordEncryptor m => ByteString -> Int -> m ByteString        
encrypt = encryptPassword pbkdf2 

verify :: ClearTextPass Validated
       -> EncryptedPass
       -> Bool
verify (ClearTextPass cp) (EncryptedPass ep) =
  TE.encodeUtf8 cp `verifyPW` TE.encodeUtf8 ep
 where
   verifyPW = verifyPasswordWith pbkdf2 (2^) 

passwordValidation :: ClearTextPass Unvalidated
                   -> OpValidation (ClearTextPass Validated)
passwordValidation (ClearTextPass pass) =
  either (failure . return) success (validatePass pass)     
  
instance Validatable (ClearTextPass Unvalidated) where   
  type Unvalidated' (ClearTextPass Unvalidated) = ClearTextPass Validated
  validate = passwordValidation  


