module Clicklac.OpFailure
  ( FailureMsg (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T 

class (Show a) => FailureMsg a where
  failMsg :: a -> Text
  
  failReason :: a -> Text
  failReason = T.pack . show   
