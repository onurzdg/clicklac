
module Database.PostgreSQL.Simple.Lifted.Notification
  ( getNotification
  , getNotificationNonBlocking
  , getBackendPID
  , Notification(..)
  ) where

import System.Posix.Types (CPid)              

import Database.PostgreSQL.Simple.Notification (Notification)       
import qualified Database.PostgreSQL.Simple.Notification as PSN
       
import Database.PostgreSQL.Simple.Lifted.PostgresClient

getNotification :: PostgresClient m => m Notification
getNotification = liftPSGClient PSN.getNotification                
                
getNotificationNonBlocking :: PostgresClient m => m (Maybe Notification)
getNotificationNonBlocking = liftPSGClient PSN.getNotificationNonBlocking 

getBackendPID :: PostgresClient m => m CPid
getBackendPID = liftPSGClient PSN.getBackendPID               
