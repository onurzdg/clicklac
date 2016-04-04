{-# LANGUAGE FlexibleContexts #-}
    
module Database.PostgreSQL.Simple.Lifted.PostgresClient where

import Control.Monad.IO.Class (MonadIO)       
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Control (MonadBaseControl(..))          
  
class (MonadIO m, MonadBaseControl IO m) => PostgresClient m where
  liftPSGClient :: (Connection -> IO b) -> m b        
