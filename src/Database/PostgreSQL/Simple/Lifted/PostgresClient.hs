
module Database.PostgreSQL.Simple.Lifted.PostgresClient where

import Control.Monad.IO.Class (MonadIO)       
import Database.PostgreSQL.Simple (Connection)
import Control.Monad.Catch (MonadCatch)
  
class (MonadIO m, MonadCatch m) => PostgresClient m where
  liftPSGClient :: (Connection -> IO b) -> m b        
