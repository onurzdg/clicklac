{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
    
module Clicklac.Validation
  ( OpValidation
  , ValidationFailure (..)
  , success
  , failure 
  , Validatable (..)
  , Validation(..)
  ) where

import Data.Either.Validation (Validation(..))
import Clicklac.OpFailure (FailureMsg(..))
       
data ValidationFailure 
  = InvalidEmail
  | InvalidUserId
  | InvalidName     
  | InvalidUsername
  | InvalidBio
  | InvalidUrl
  | InvalidLocation     
  | PasswordLong
  | PasswordShort
  deriving (Eq, Show)    
   
instance FailureMsg ValidationFailure where
  failMsg InvalidEmail     = "Invalid email address"
  failMsg InvalidUserId    = "Invalid user id"
  failMsg InvalidName      = "Invalid name"
  failMsg InvalidUsername  = "Invalid username"
  failMsg InvalidBio       = "Invalid bio"
  failMsg InvalidUrl       = "Invalid URL"
  failMsg InvalidLocation  = "Invalid location"
  failMsg PasswordLong     = "Must have at most 100 characters"
  failMsg PasswordShort    = "Must have at least 6 characters"

success :: a -> Validation e a
success = Success

failure :: e -> Validation e a        
failure = Failure        

type OpValidation a = Validation[ValidationFailure] a        
   
class Validatable a where
  type Unvalidated' a :: *    
  validate :: a -> OpValidation (Unvalidated' a)
