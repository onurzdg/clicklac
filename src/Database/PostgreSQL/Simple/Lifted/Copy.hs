    
module Database.PostgreSQL.Simple.Lifted.Copy
  ( copy 
  , copy_
  , getCopyData
  , putCopyData
  , putCopyEnd
  , putCopyError
  , CopyOutResult(..)
  ) where

import Data.ByteString (ByteString)       
import Data.Int (Int64)       

import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple (Query)
import Database.PostgreSQL.Simple.Copy (CopyOutResult)      
import qualified Database.PostgreSQL.Simple.Copy as C

import Database.PostgreSQL.Simple.Lifted.PostgresClient   

copy :: (PostgresClient m, ToRow params) => Query -> params -> m ()
copy template qs = liftPSGClient (\c -> C.copy c template qs)

copy_ :: (PostgresClient m) => Query -> m ()
copy_ template = liftPSGClient (`C.copy_` template)     
  
getCopyData :: (PostgresClient m) => m CopyOutResult
getCopyData = liftPSGClient C.getCopyData

putCopyData :: (PostgresClient m) => ByteString -> m ()
putCopyData d = liftPSGClient (`C.putCopyData` d)              
       
putCopyEnd :: (PostgresClient m) => m Int64
putCopyEnd = liftPSGClient C.putCopyEnd

putCopyError :: (PostgresClient m) => ByteString -> m ()
putCopyError err = liftPSGClient (`C.putCopyError` err)             
