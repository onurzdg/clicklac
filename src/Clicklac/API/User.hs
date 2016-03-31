{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}    
{-# LANGUAGE FlexibleInstances #-}        
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
    
module Clicklac.API.User where       

import Data.Text (Text)       
import qualified Data.Text as T
       
import Servant
import qualified Data.Either.Validation as V

import Clicklac.Application (App)
import Clicklac.Servant.Combinator
  ( AuthProtected
  , NotAuthProtected
  , WithUserId
  )
import qualified Clicklac.Servant.Response as RS  
import Clicklac.Session (deleteSessions)  
import Clicklac.Types (UserId)       
import Clicklac.User
  ( Account (..)
  , InitState(..)
  , AccountStateUpdate(..)
  , AccountState(..)
  , NewAccount
  , Profile
  , getUserAccById  
  , newAccount
  , updateProfile
  , updateAccountState
  ) 
import Clicklac.Validation       

type UserAPI = "user" :> UserAPI'      
type UserAPI' =
    AuthProtected :> Capture "userid" UserId
    :> Get '[JSON] (Maybe Account)
  :<|>  
    NotAuthProtected :> ReqBody '[JSON] (OpValidation(NewAccount 'UnInit))
    :> PostCreated '[JSON] (NewAccount 'Init)
  :<|>  
    AuthProtected :> WithUserId :> ReqBody '[JSON] (OpValidation Profile)
    :> Put '[JSON] (Profile)
  :<|>
    AuthProtected :> Capture "userid" UserId :> "account" :> "state"
    :> ReqBody '[JSON] (AccountStateUpdate) :> PutNoContent '[JSON] () -- should be called by an admin
  
userAPI :: ServerT UserAPI App
userAPI = userAcc
     :<|> createUser
     :<|> updateProfile'
     :<|> updateAccountState'
        
userAcc :: UserId -> App (Maybe Account)
userAcc = getUserAccById

accCreateFailMsg :: Text        
accCreateFailMsg = T.pack "Unable to create new account"       
        
createUser :: OpValidation (NewAccount 'UnInit)
           -> App (NewAccount 'Init)
createUser (V.Failure xs) = RS.s400 accCreateFailMsg xs 
createUser (V.Success nu) = do
  res <- newAccount nu
  case res of
    Right ni -> return ni
    Left xs -> RS.s409 accCreateFailMsg xs

profUpdateFailMsg :: Text        
profUpdateFailMsg = T.pack "Unable to update profile"         
  
updateProfile' :: UserId -> OpValidation Profile -> App Profile
updateProfile' _ (V.Failure xs) = RS.s400 profUpdateFailMsg xs     
updateProfile' uid (V.Success prof) = do
  res <- updateProfile uid prof             
  case res of
    Right prof' -> return prof'
    Left xs -> RS.s409 profUpdateFailMsg xs 

-- Should be only called by someone with admin priviliges  
updateAccountState' :: UserId -> AccountStateUpdate -> App ()
updateAccountState' uid ac@(ASU Active) = updateAccountState uid ac            
updateAccountState' uid ac@(ASU Suspended) = 
  deleteSessions uid >> updateAccountState uid ac
