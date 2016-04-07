
module Clicklac.Middleware.AuthCheck where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI       
import Data.Maybe (isNothing)
import qualified Data.Text.Encoding as TE       
       
import Network.Wai
  ( Middleware 
  , requestHeaders
  , vault
  )  
import qualified Database.CQL.IO as CQ (ClientState)       
import Web.Cookie (parseCookies)
import Network.HTTP.Types.Header (hCookie, hAuthorization)
import qualified Data.Vault.Lazy as VT

import Clicklac.Cookie
  ( expiredCookieHeaderBS
  , sessionIdCookieKey
  )              
import Clicklac.Session
  ( ssId
  , ssUserId
  , getSession'
  , validateSessionId
  , sessionIdT
  , ssUserId
  , isSessionValid
  )
  
import Clicklac.Types (UserId)
import Clicklac.User (updateLastUserActivity)       
       
authCheck :: CQ.ClientState -> VT.Key UserId -> Middleware
authCheck dbState vkey app req respond = do
  let reqH = requestHeaders req        
      mauthH = lookup hAuthorization reqH 
      mcookieH = lookup hCookie reqH
      msid = extractSid mauthH mcookieH
  case msid of
    Nothing -> app req respond
    Just sid -> do
      msess <- getSession' dbState sid 
      case msess of
        Just sess -> do
          sessValid <- isSessionValid sess
          if sessValid
            then do -- if session cookie used, add its value to req header
              let sidBS =  TE.encodeUtf8 . sessionIdT $ sid
                  reqH' = if isNothing mauthH
                            then (hAuthorization, sidBS) : reqH
                            else reqH
                  -- | Put user id into the vault to avoid making extra DB hits
                  --   in the handlers 
                  vault' = VT.insert vkey (ssUserId sess) (vault req)
                  req' = req {requestHeaders = reqH', vault = vault'}          
              updateLastUserActivity dbState (ssUserId sess) (ssId sess)
              app req' respond
          else app (expireTokens req) respond
        Nothing ->  -- session not found in DB
          app (expireTokens req) respond
 where             
   extractSid (Just autHSessId) _ = validateSessionId autHSessId
   extractSid _  (Just cookieH) =  
     validateSessionId =<< lookup sessionIdCookieKey (parseCookies cookieH)
   extractSid _ _ = Nothing

   expireTokens req' =
     req' {requestHeaders = expireSessCookie $ requestHeaders req'}    
  
   expireSessCookie reqH = (CI.mk . BS8.pack $ "set-cookie",
                           expiredCookieHeaderBS sessionIdCookieKey) : reqH
