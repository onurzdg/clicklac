    
module Data.Aeson.TH.Extra where

import Data.Aeson.TH
  ( defaultOptions
  , Options(..)
  )

import Data.String.Extra (dropL1)
       
prefixRemovedOpts :: Int -> Options        
prefixRemovedOpts i = defaultOptions {fieldLabelModifier = dropL1 i}
    
