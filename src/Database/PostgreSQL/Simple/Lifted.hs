   
module Database.PostgreSQL.Simple.Lifted
 (  
 -- * query 
   query
 , query_
 , queryWith
 , queryWith_
 , returning
 , returningWith
 
 -- * execute
 , execute
 , execute_
 , executeMany
 
 -- * fold
 , fold
 , fold_
 , foldWithOptions
 , foldWithOptions_
 , foldWithOptionsAndParser
 , foldWithOptionsAndParser_
 , foldWith
 , foldWith_
 
 -- * forEach
 , forEach
 , forEach_
 , forEachWith
 , forEachWith_

 -- * transaction
 , PST.withTransaction
 , PST.withTransactionLevel
 , PST.withTransactionMode
 , PST.withSavepoint
 , PST.begin
 , PST.beginMode
 , PST.rollback

 -- * helper functions
 , formatMany
 , formatQuery

 -- * re-import client
 , module Database.PostgreSQL.Simple.Lifted.PostgresClient  
 ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)       
       
import Database.PostgreSQL.Simple.ToRow (ToRow)
import Database.PostgreSQL.Simple.FromRow
 ( FromRow
 , RowParser
 )
import Database.PostgreSQL.Simple
 ( Query
 , FoldOptions
 )       
import qualified Database.PostgreSQL.Simple as PS       
import qualified Database.PostgreSQL.Simple.Lifted.Transaction as PST
import Database.PostgreSQL.Simple.Lifted.PostgresClient          

query :: (PostgresClient m, ToRow q, FromRow r)
      => Query -> q -> m [r]
query q params = liftPSGClient (\c -> PS.query c q params)    

query_ :: (PostgresClient m, FromRow r) => Query -> m [r]
query_ q = liftPSGClient (`PS.query_` q)

queryWith :: (PostgresClient m, ToRow q)
          => RowParser r -> Query -> q -> m [r]
queryWith p q t = liftPSGClient (\c -> PS.queryWith p c q t)

queryWith_ :: (PostgresClient m)
           => RowParser r -> Query -> m [r]
queryWith_ p q = liftPSGClient (\c -> PS.queryWith_ p c q)          

fold :: (PostgresClient m, FromRow row, ToRow params)
     => Query
     -> params
     -> b
     -> (b -> row -> IO b)
     -> m b
fold template qs a f = liftPSGClient (\c -> PS.fold c template qs a f)

fold_ :: (PostgresClient m, FromRow row)
      => Query -> b -> (b -> row -> IO b) -> m b
fold_ template a f = liftPSGClient (\c -> PS.fold_ c template a f)     
          
foldWithOptions :: (PostgresClient m, FromRow row, ToRow params)
                => FoldOptions
                -> Query
                -> params
                -> b
                -> (b -> row -> IO b)
                -> m b
foldWithOptions opts template qs a f =
  liftPSGClient (\c -> PS.foldWithOptions opts c template qs a f)

foldWithOptions_ :: (PostgresClient m, FromRow row)
                 => FoldOptions
                 -> Query
                 -> b
                 -> (b -> row -> IO b)
                 -> m b
foldWithOptions_ opts template a f =
  liftPSGClient (\c -> PS.foldWithOptions_ opts c template a f)


foldWithOptionsAndParser :: (PostgresClient m, ToRow params) 
                         => FoldOptions
                         -> RowParser row
                         -> Query
                         -> params -> a -> (a -> row -> IO a) -> m a
foldWithOptionsAndParser opts parser q params a f  =
  liftPSGClient (\c -> PS.foldWithOptionsAndParser opts parser c q params a f)

foldWithOptionsAndParser_ :: (PostgresClient m) 
                          => FoldOptions
                          -> RowParser r
                          -> Query             
                          -> a                 
                          -> (a -> r -> IO a) 
                          -> m a
foldWithOptionsAndParser_ opts parser q a f =
  liftPSGClient (\c -> PS.foldWithOptionsAndParser_ opts parser c q a f)

foldWith :: (PostgresClient m, ToRow params)
         => RowParser row        
         -> Query
         -> params
         -> a
         -> (a -> row -> IO a)
         -> m a
foldWith = foldWithOptionsAndParser PS.defaultFoldOptions

foldWith_ :: (PostgresClient m)
          => RowParser r         
          -> Query
          -> a
          -> (a -> r -> IO a)
          -> m a
foldWith_ = foldWithOptionsAndParser_ PS.defaultFoldOptions       
                
forEach :: (PostgresClient m, FromRow r, ToRow q)
        => Query -> q -> (r -> IO ()) -> m ()
forEach template qs f = liftPSGClient (\c -> PS.forEach c template qs f)

forEach_ :: (PostgresClient m, FromRow r)
         => Query -> (r -> IO ()) -> m ()
forEach_ template f = liftPSGClient (\c -> PS.forEach_ c template f)

returning :: (PostgresClient m, ToRow q, FromRow r)
          => Query -> [q] -> m [r]
returning q params = liftPSGClient (\c -> PS.returning c q params) 

execute :: (PostgresClient m, ToRow q)
        => Query -> q -> m Int64
execute template qs = liftPSGClient (\c -> PS.execute c template qs)         

execute_ :: (PostgresClient m)
         => Query -> m Int64
execute_ template = liftPSGClient (`PS.execute_` template)        

executeMany :: (PostgresClient m, ToRow q)
            => Query -> [q] -> m Int64
executeMany template qs = liftPSGClient (\c -> PS.executeMany c template qs)

forEachWith :: (PostgresClient m, ToRow q)
            => RowParser r 
            -> Query
            -> q
            -> (r -> IO ())
            -> m ()
forEachWith parser q qs = foldWith parser q qs () . const

forEachWith_ :: (PostgresClient m)
             => RowParser r
             -> Query
             -> (r -> IO ())
             -> m ()
forEachWith_ parser q = foldWith_ parser q () . const

returningWith :: (PostgresClient m, ToRow q)
              => RowParser r
              -> Query
              -> [q]
              -> m [r]
returningWith parser q qs = liftPSGClient (\c -> PS.returningWith parser c q qs)
             

formatMany :: (PostgresClient m, ToRow q)
           => Query -> [q] -> m ByteString
formatMany q qs = liftPSGClient (\c -> PS.formatMany c q qs)  

formatQuery :: (PostgresClient m, ToRow q)
            => Query -> q -> m ByteString
formatQuery q qs = liftPSGClient (\c -> PS.formatQuery c q qs)           
