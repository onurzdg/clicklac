{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}            
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}    
    
module Clicklac.Password
  ( Password(UnValidatedPass)
  , PWState (..)
  , clearTextPass
  , getClearText
  , toEncrypted
  , encrypt'
  , verify
  ) where

import Control.Monad (liftM)       
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T (length)       
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import System.IO.Unsafe (unsafePerformIO)
      
import Database.CQL.Protocol
  ( Cql(..)
  , Tagged(..)
  , ColumnType (TextColumn)
  , Value(CqlText)
  )
import Crypto.PasswordStore (verifyPasswordWith, pbkdf2, makePasswordWith)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types (Value(String), typeMismatch)       

import Clicklac.Types (PasswordEncryptor(..))       
import Clicklac.Validation
  ( Validatable(..)
  , ValidationFailure(PasswordLong, PasswordShort)
  , failure
  , success
  , OpValidation
  )           
      
data PWState 
  = Encrypted -- ^ Validated
  | ClearText -- ^ Validated
  | PWUnvalidated

data Password :: PWState -> * where 
   EncryptedPass :: Text
                 -> Password 'Encrypted     
   ClearTextPass :: Text
                 -> Password 'ClearText
   UnValidatedPass :: Text
                   -> Password 'PWUnvalidated
     
instance Eq (Password 'ClearText) where
  (ClearTextPass cp1) == (ClearTextPass cp2) = cp1 == cp2

instance Show (Password 'ClearText) where
  show _ = ""

instance Show (Password 'Encrypted) where
  show _ = ""

instance Show (Password 'PWUnvalidated) where
  show _ = ""               

instance FromJSON (Password 'PWUnvalidated) where
  parseJSON (String p) = pure $ UnValidatedPass p   
  parseJSON unknown = typeMismatch "Password PWU" unknown

instance ToJSON (Password 'PWUnvalidated) where
  toJSON (UnValidatedPass p) = String p           

instance FromJSON (Password 'ClearText) where
  parseJSON (String p) = pure $ ClearTextPass p   
  parseJSON unknown = typeMismatch "ClearTextPass" unknown  

instance Cql (Password 'Encrypted) where
  ctype = Tagged TextColumn
  toCql (EncryptedPass p) = CqlText p
  fromCql (CqlText p) = Right (EncryptedPass p)
  fromCql _           = Left "Expected CqlText"      

defaultStrength :: Int
defaultStrength = 15
                
toEncrypted :: (PasswordEncryptor m)
            => Password 'ClearText
            -> m (Password 'Encrypted)
toEncrypted (ClearTextPass p) = (EncryptedPass . TE.decodeUtf8) `liftM`
    (encrypt (TE.encodeUtf8 p) defaultStrength)

getClearText :: Password 'ClearText -> Text   
getClearText (ClearTextPass p) = p    

clearTextPass :: Password 'PWUnvalidated
              -> Either ValidationFailure (Password 'ClearText)
clearTextPass (UnValidatedPass pass) 
  | pwLength > 100 = Left PasswordLong
  | pwLength < 6 = Left PasswordShort
  | otherwise = Right $ ClearTextPass pass
  where
    pwLength = (T.length pass)    

-- Do not use in actual code: exposed to satify the documenent generator
encrypt' :: ByteString -> Password 'Encrypted  
encrypt' pass = EncryptedPass . TE.decodeUtf8 $
  unsafePerformIO $ makePasswordWith pbkdf2 pass defaultStrength 
  
encrypt :: PasswordEncryptor m => ByteString -> Int -> m ByteString        
encrypt pass strength = encryptPassword pbkdf2 pass strength    

verify :: Password 'ClearText
       -> Password 'Encrypted
       -> Bool
verify (ClearTextPass cp) (EncryptedPass ep) =
  TE.encodeUtf8 cp `verifyPW` TE.encodeUtf8 ep
  where
    verifyPW pass hash = verifyPasswordWith pbkdf2 (2^) pass hash

passwordValidation :: Password 'PWUnvalidated
                   -> OpValidation (Password 'ClearText)
passwordValidation pass =
  either (failure . return)
         success
         (clearTextPass pass)     
  
instance Validatable (Password 'PWUnvalidated) where   
  type Unvalidated (Password 'PWUnvalidated) = Password 'ClearText
  validate = passwordValidation  
