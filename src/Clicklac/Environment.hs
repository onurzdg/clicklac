
module Clicklac.Environment where

import Control.Monad.IO.Class (MonadIO(..))
import System.Environment (lookupEnv)       
import Text.Read (readMaybe)              
       
data AppEnvironment 
  = Development
  | Production
  deriving (Show, Eq, Read)
 
lookupAppEnv :: (MonadIO m) => m AppEnvironment
lookupAppEnv = lookupEnvVar "APP_ENV" Development             

lookupEnvVar :: (MonadIO m, Read a) => String -> a -> m a
lookupEnvVar env def' = fmap readEnvVar (liftIO $ lookupEnv env)
  where
   readEnvVar (Just envStr) = maybe def' id (readMaybe envStr)
   readEnvVar _ = def'               
