{-# LANGUAGE FlexibleContexts #-}
    
module Database.PostgreSQL.Simple.Lifted.PostgresClient where
       
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Trans.Control (MonadBaseControl(..))          
  
class (MonadBaseControl IO m) => PostgresClient m where
  liftPSGClient :: (Connection -> IO b) -> m b        
