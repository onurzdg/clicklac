
module Clicklac.Environment
  ( AppEnvironment(..)
  , EnvLookup(..)
  , lookupAppEnv'
  , lookupEnvVar'  
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)       
import Text.Read (readMaybe)              
       
data AppEnvironment 
  = Development
  | Production
  deriving (Show, Eq, Read)


appEnv :: String
appEnv = "APP_ENV"

class (Monad m) => EnvLookup m where
  lookupAppEnv :: m AppEnvironment
  lookupAppEnv = lookupEnvVar appEnv Development             
  lookupEnvVar :: Read a => String -> a -> m a
 
lookupAppEnv' :: (MonadIO m) => m AppEnvironment
lookupAppEnv' = lookupEnvVar' appEnv Development             

lookupEnvVar' :: (MonadIO m, Read a) => String -> a -> m a
lookupEnvVar' env def' = fmap readEnvVar (liftIO $ lookupEnv env)
 where
  readEnvVar (Just envStr) = fromMaybe def' (readMaybe envStr)
  readEnvVar _ = def'               
