{-# LANGUAGE FlexibleContexts #-}
    
module Database.PostgreSQL.Simple.Lifted.Transaction
  (
  -- * Transaction handling
    withTransaction
  , withTransactionLevel
  , withTransactionMode
  , withTransactionSerializable
  , withTransactionModeRetry
  , begin
  , beginMode
  , beginLevel
  , commit
  , rollback

  -- * Defaults
  , PS.defaultTransactionMode
  , PS.defaultIsolationLevel
  , PS.defaultReadWriteMode
  
  -- * Savepoint
  , withSavepoint
  , releaseSavepoint
  , rollbackToSavepoint
  , rollbackToAndReleaseSavepoint
  , newSavepoint
  
  -- * Predicates
  , PS.isSerializationError
  , PS.isNoActiveTransactionError
  , PS.isFailedTransactionError
  
  -- * Data types
  , TransactionMode(..)
  , IsolationLevel(..)
  , ReadWriteMode(..)
  ) where

import Control.Exception (SomeException, fromException)
import Control.Exception.Lifted 
  ( mask
  , onException
  , catch
  , throwIO
  , try
  ) 

import Database.PostgreSQL.Simple (SqlError)
import Database.PostgreSQL.Simple.Types (Savepoint)       
import Database.PostgreSQL.Simple.Transaction
  ( TransactionMode(..)
  , IsolationLevel
  , ReadWriteMode
  ) 
import qualified Database.PostgreSQL.Simple.Transaction as PS       

import Database.PostgreSQL.Simple.Lifted.PostgresClient
       
withTransaction :: (PostgresClient m)
                => m a -> m a
withTransaction = withTransactionMode PS.defaultTransactionMode

withTransactionLevel :: (PostgresClient m)
                     => IsolationLevel -> m a -> m a
withTransactionLevel lvl =
  withTransactionMode PS.defaultTransactionMode { PS.isolationLevel = lvl }

begin :: (PostgresClient m) => m ()
begin = liftPSGClient $ PS.beginMode PS.defaultTransactionMode

beginLevel :: (PostgresClient m) => IsolationLevel -> m ()
beginLevel = liftPSGClient . PS.beginLevel           

beginMode :: (PostgresClient m) => TransactionMode -> m ()
beginMode = liftPSGClient . PS.beginMode

commit :: (PostgresClient m) => m ()
commit = liftPSGClient PS.commit

rollback :: (PostgresClient m) => m ()
rollback = liftPSGClient PS.commit           

withTransactionMode :: (PostgresClient m)
                    => TransactionMode -> m a -> m a
withTransactionMode mode act =
  mask $ \restore -> do                   
    beginMode mode
    r <- restore act `onException` rollback
    commit
    return r                  

withTransactionSerializable :: (PostgresClient m)
                            => m a -> m a
withTransactionSerializable =
  withTransactionModeRetry
    TransactionMode
    { isolationLevel = PS.Serializable
    , readWriteMode  = PS.ReadWrite
    }
    PS.isSerializationError  

withTransactionModeRetry :: (PostgresClient m)
                         => TransactionMode
                         -> (SqlError -> Bool)
                         -> m a
                         -> m a
withTransactionModeRetry mode shouldRetry act =
  mask $ \restore -> 
   retryLoop $ try $ do
     a <- restore act
     commit
     return a
 where
    retryLoop :: (PostgresClient m)
              => m (Either SomeException a) -> m a
    retryLoop act' = do
      beginMode mode
      r <- act'
      case r of
        Left e -> do
          rollback 
          case fmap shouldRetry (fromException e) of
            Just True -> retryLoop act'
            _ -> throwIO e
        Right a -> return a  

withSavepoint :: (PostgresClient m)
              => m a -> m a
withSavepoint body =
  mask $ \restore -> do              
    sp <- newSavepoint 
    r <- restore body `onException` rollbackToAndReleaseSavepoint sp
    releaseSavepoint sp `catch` \err ->
      if PS.isFailedTransactionError err
       then rollbackToAndReleaseSavepoint sp
       else throwIO err
    return r 
  
releaseSavepoint :: (PostgresClient m)
                 => Savepoint
                 -> m ()        
releaseSavepoint s = liftPSGClient (`PS.releaseSavepoint` s)
                 
rollbackToSavepoint :: (PostgresClient m)
                    => Savepoint
                    -> m ()
rollbackToSavepoint s = liftPSGClient (`PS.rollbackToSavepoint` s)
                    
rollbackToAndReleaseSavepoint :: (PostgresClient m)
                              => Savepoint
                              -> m ()
rollbackToAndReleaseSavepoint s =
  liftPSGClient (`PS.rollbackToAndReleaseSavepoint` s)           
  
newSavepoint :: (PostgresClient m) => m Savepoint
newSavepoint = liftPSGClient PS.newSavepoint                      
