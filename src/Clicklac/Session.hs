
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Clicklac.Session
  ( Session
  , SessionId
  , sessionIdT

  -- * Session Accessors
  , ssUserId
  , ssId
  , ssStartAt
  , ssEndAt
  , ssHost
  , ssIp

  -- * Session Utils
  , validateSessionId
  , isSessionValid

  -- * DB Ops
  , createSession
  , getSession
  , getSession'
  , deleteSession
  , deleteSessions
  ) where

import Control.Monad (guard, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson
  ( ToJSON(..)
  , toJSON
  , (.=)
  , object
  )
import Data.Aeson.TH (defaultOptions, deriveToJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity(..))
import Data.String.Extra (unlines')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Control.Concurrent.Async.Lifted (concurrently)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Time.Clock as Clock
  ( getCurrentTime
  , addUTCTime
  )
import qualified Database.CQL.IO as CQ
  ( PrepQuery
  , ClientState
  , addPrepQuery
  , batch
  , prepared
  , query
  , query1
  , setType
  , write
  , runClient
  )
import Database.CQL.Protocol
  ( Cql (..)
  , BatchType (BatchLogged)
  , Tagged (..)
  , ColumnType (TextColumn)
  , Value(CqlText)
  , TupleType
  , QueryParams
  , Record (..)
  , W
  , R
  , recordInstance
  )
import Database.CQL.Protocol.Extra (defQueryParams)
import Servant (FromHttpApiData(..))

import Clicklac.Types
  ( CassClient(..)
  , TimeGetter(..)
  , NonceGenerator(..)
  , HostnameGetter(..)
  , UserId
  )

newtype SessionId = SI Text
  deriving (Eq, Show)

instance FromHttpApiData SessionId where
  parseUrlPiece = Right . SI -- no need to validate, AuthCheck.hs does it

deriveToJSON defaultOptions ''SessionId

sessionIdT :: SessionId -> Text
sessionIdT (SI t) = t

instance Cql SessionId where
  ctype = Tagged TextColumn
  toCql   (SI u) = CqlText u
  fromCql (CqlText u) = Right (SI u)
  fromCql _           = Left "Expected CqlText"

data Session = Session
  { ssId      :: !SessionId
  , ssUserId  :: !UserId
  , ssStartAt :: !UTCTime
  , ssEndAt   :: !UTCTime
  , ssHost    :: !Text
  , ssIp      :: !(Maybe Text)
  } deriving (Show)

instance ToJSON Session where
  toJSON Session{..} = object[
    "id" .= ssId, "userId" .= ssUserId,
    "startAt" .= ssStartAt, "endAt" .= ssEndAt]

recordInstance ''Session

-- inspired by https://goo.gl/28IVJ4
generateSessionId :: (NonceGenerator m) => m SessionId
generateSessionId = fmap SI generateNonce

validateSessionId :: ByteString -> Maybe SessionId
validateSessionId sidBS = do
  let text = TE.decodeUtf8 sidBS
  guard (T.length text == 24)
  decoded <- either (const Nothing) Just $ B64URL.decode (TE.encodeUtf8 text)
  guard (BS.length decoded == 18)
  return $ SI text

isSessionValid :: (MonadIO m) => Session -> m Bool
isSessionValid sess = do
  curT <- liftIO Clock.getCurrentTime
  return $ curT < ssEndAt sess

------- DB Ops ------------------------------------------
createSession :: (CassClient m, NonceGenerator m, HostnameGetter m, TimeGetter m)
              => UserId
              -> Maybe Text
              -> m Session
createSession uid ipAddr = do
  sessId <- generateSessionId
  curT <- getCurrentTime
  let expirationTime = Clock.addUTCTime (fromIntegral duration) curT
  hostname <- getHostname
  let sess = Session sessId uid curT expirationTime hostname ipAddr
  runCassOp $ CQ.batch $ CQ.setType BatchLogged >>
              CQ.addPrepQuery qs (asTuple sess) >>
              CQ.addPrepQuery qus (asTuple sess)
  return sess
 where
   qs :: CQ.PrepQuery W (TupleType Session) ()
   qs = CQ.prepared $ unlines'
     ["insert into session (session_id, user_id, start_at,"
     ,"end_at, host, remote_ip) values"
     ,"(?,?,?,?,?,?) USING TTL"
     ,show duration]

   qus :: CQ.PrepQuery W (TupleType Session) ()
   qus = CQ.prepared $ unlines'
     ["insert into user_session (session_id, user_id, start_at, "
     ,"end_at,  host, remote_ip) values (?,?,?,?,?,?) USING TTL "
     ,show duration]

   duration = 157680000 :: Int  -- 5 years in seconds

getSession' :: MonadIO m => CQ.ClientState -> SessionId -> m (Maybe Session)
getSession' s sid =
  fmap (fmap asRecord) . CQ.runClient s . CQ.query1 sessQ $ sessP sid

getSession :: (CassClient m)
           => SessionId
           -> m (Maybe Session)
getSession sid = fmap (fmap asRecord) . runCassOp . CQ.query1 sessQ $ sessP sid

sessQ :: CQ.PrepQuery R (Identity SessionId) (TupleType Session)
sessQ = CQ.prepared "select session_id, user_id, start_at, end_at, \
          \ host, remote_ip from session where session_id =?"

sessP :: SessionId -> QueryParams (Identity SessionId)
sessP sid = defQueryParams (Identity sid)

getSessions :: (CassClient m)
            => UserId
            -> m [Session]
getSessions uid = fmap (fmap asRecord) . runCassOp $ CQ.query q p
  where
    q :: CQ.PrepQuery R (Identity UserId) (TupleType Session)
    q = CQ.prepared "select session_id, user_id, start_at, end_at, \
          \ host, remote_ip from user_session where user_id =?"

    p :: QueryParams (Identity UserId)
    p = defQueryParams (Identity uid)

deleteSession :: (CassClient m, MonadBaseControl IO m)
              => UserId
              -> SessionId
              -> m ()
deleteSession uid sid =
  runCassOp $ CQ.batch $ CQ.setType BatchLogged >>
              CQ.addPrepQuery qsDel (Identity sid) >>
              CQ.addPrepQuery qusDel (uid, sid)
 where
   qsDel :: CQ.PrepQuery W (Identity SessionId) ()
   qsDel = CQ.prepared "delete from session where session_id =?"

   qusDel :: CQ.PrepQuery W (UserId, SessionId) ()
   qusDel = CQ.prepared
     "delete from user_session where user_id =? and session_id =?"

-- Deletes all sessions of a user
deleteSessions :: (CassClient m, MonadBaseControl IO m) => UserId -> m ()
deleteSessions uid = do
  sessIds <- fmap ssId <$> getSessions uid
  let us = runCassOp $ CQ.write qus pus
  void $ foldr conc (return []) (us:map (runCassOp . CQ.write qs . ps) sessIds)

 where
   qus :: CQ.PrepQuery W (Identity UserId) a
   qus = CQ.prepared "delete from user_session where user_id =?"

   pus :: QueryParams (Identity UserId)
   pus = defQueryParams (Identity uid)

   qs :: CQ.PrepQuery W (Identity SessionId) a
   qs = CQ.prepared "delete from session where session_id =?"

   ps :: SessionId -> QueryParams (Identity SessionId)
   ps sid = defQueryParams (Identity sid)

   conc ioa ioas = do
     (a,as) <- concurrently ioa ioas
     return (a:as)
