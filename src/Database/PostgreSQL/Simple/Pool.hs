{-# LANGUAGE RecordWildCards #-}
        
module Database.PostgreSQL.Simple.Pool
  ( ConnectionString
  , PoolConfig(..)
  , PostgresPool
  , createConnPool
  , destroyConnPool
  , withConnPool
  , defaultPoolConfig
  ) where

import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO(..))
       
import Data.Pool (Pool (..))
import qualified Data.Pool as P       
import qualified Database.PostgreSQL.Simple as PS
       
type ConnectionString = ByteString
       
data PoolConfig = PoolConfig
  { pcConnStr    :: ConnectionString
  , pcNumStripes :: Int
  , pcIdleTime   :: Double
  , pcResources  :: Int
  }
       
data PostgresPool = PostgresPool (Pool PS.Connection)                

createConnPool :: (MonadIO m) => PoolConfig -> m PostgresPool
createConnPool PoolConfig{..} = do
  connPool <- liftIO $
    P.createPool (PS.connectPostgreSQL pcConnStr) PS.close
                 pcNumStripes (realToFrac pcIdleTime) pcResources
  return $ PostgresPool connPool

withConnPool :: MonadIO m => PostgresPool -> (PS.Connection -> IO b) -> m b
withConnPool (PostgresPool p) f = liftIO $ P.withResource p f 

destroyConnPool :: MonadIO m => PostgresPool -> m ()
destroyConnPool (PostgresPool p) = liftIO $ P.destroyAllResources p

defaultPoolConfig :: ConnectionString -> PoolConfig
defaultPoolConfig poolConf = PoolConfig poolConf 1 5 (60 * 10)           
