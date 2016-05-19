{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}    
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Clicklac.Servant.Combinator where

import Data.Maybe (fromJust)

import qualified Data.Vault.Lazy as V      
import Network.Wai (vault)
import Servant (Proxy(..), (:>))
import Servant.Server.Internal
      
import Clicklac.Application (AppConfig(..))
import Clicklac.Types (UserId)

-- Responses in these combinators pass through 'API.toApplication'       

-- Most APIs should be accessed by authenticated users
data AuthProtected 
       
instance (HasContextEntry context AppConfig, HasServer rest context)
  => HasServer (AuthProtected :> rest) context where
  
  type ServerT (AuthProtected :> rest) m = ServerT rest m
  
  route Proxy context subserver = WithRequest $ \request -> 
    route (Proxy :: Proxy rest) context 
      (addAcceptCheck subserver $ authCheck request)
     where
       authCheck req = do 
         let appConf :: AppConfig = getContextEntry context
             vKey = vaultKey appConf
         case V.lookup vKey (vault req) of
           Just _ -> return $ Route ()
           Nothing -> return $ -- user id not in the vault: no session exists
             FailFatal err401 {errBody = "Invalid auth header"}

-- | There are API endpoints (e.g, login) that should be only accessed when
--   the user is not authenticated yet. Those APIs should reject requests
--   if the user is already authenticated.        
data NotAuthProtected 

instance (HasContextEntry context AppConfig, HasServer rest context)
  => HasServer (NotAuthProtected :> rest) context where
  
  type ServerT (NotAuthProtected :> rest) m = ServerT rest m
  
  route Proxy context subserver = WithRequest $ \request -> 
    route (Proxy :: Proxy rest) context 
      (addAcceptCheck subserver $ authCheck request)
     where
       authCheck req = do
         let appConf :: AppConfig = getContextEntry context
             vKey = vaultKey appConf
         case V.lookup vKey (vault req) of
           Just _ -> return $
             FailFatal err401 { errBody = "Already authenticated"
                              , errReasonPhrase = "AlreadyAuthenticated"
                              } 
           Nothing -> return $ Route ()


-- Pass user id to the API after looking it up in the vault  
data WithUserId

instance (HasContextEntry context AppConfig, HasServer rest context)
  => HasServer (WithUserId :> rest) context where
  
    type ServerT (WithUserId :> rest) m = UserId -> ServerT rest m

    route Proxy context subserver = WithRequest $ \req -> do
       let appConf :: AppConfig = getContextEntry context
       let vKey = vaultKey appConf
       -- | 'AuthCheck' middleware and 'AuthProtected' combinator guarantee the
       --   presence of UserId in vault
       let uid = fromJust $ V.lookup vKey (vault req) 
       route (Proxy :: Proxy rest) context $ passToServer subserver uid

