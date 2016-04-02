{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}    
{-# LANGUAGE FlexibleContexts          #-}    
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}    
{-# LANGUAGE TypeOperators             #-}        

module Clicklac.Servant.Response
  ( 
  -- * 400-499 HTTP responses
    s400
  , s401
  , s403
  , s404
  , s409
  
  -- * 500-514 HTTP responses
  , s500
  , s503

  -- * Utils
  , encodeErr
  , contentType

  -- * Failure
  , RequestFailure(..)
  
  -- * Headers
  , Location(..)
  ) where       

import Control.Monad.Error.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.List (foldl')       
import Data.Text (Text)

import Data.Aeson (encode, ToJSON(..), (.=), object)
import Data.ByteString.Conversion.To (ToByteString(..))              
import Network.HTTP.Types.Header (hContentType, HeaderName)         
import Servant.Server.Internal.ServantErr
import Servant.Docs (ToSample(..), singleSample)       
       
import Clicklac.OpFailure (FailureMsg(..))

data RequestFailure
  = UrlNotFound
  | MethodNotAllowed
  | InvalidRequestBody
  | UnsupportedMedia
  | BadRequest
  | InternalError
  | ResourceNotFound
  deriving (Eq, Show)

instance FailureMsg RequestFailure where
  failMsg UrlNotFound           = "Url Not Found"
  failMsg MethodNotAllowed      = "Method Not Allowed"
  failMsg InvalidRequestBody    = "Invalid Request Body"
  failMsg UnsupportedMedia      = "Unsupported Media Type"
  failMsg BadRequest            = "Bad Request"
  failMsg InternalError         = "Internal Error"
  failMsg ResourceNotFound      = "Resource Not Found"     

data Err = Err
  { msg    :: !Text
  , reason :: !Text
  } deriving Show

instance ToJSON Err where
  toJSON(Err{..}) =
    object ["message" .= msg, "reason" .= reason]
             
data ErrJSON = forall j. ToJSON j =>
  ErrJSON {_code :: !Int, _message :: !Text, _errors :: ![j]}

instance ToJSON ErrJSON where
  toJSON(ErrJSON{..}) =
    object ["code" .= _code, "message" .= _message, "errors" .= _errors]
             
-- similiar to https://goo.gl/qSTbGa
newtype ErrJSONRoot = ErrJSONRoot ErrJSON

instance ToJSON ErrJSONRoot where
  toJSON(ErrJSONRoot errJSON) = object ["error" .= errJSON]        

encodeErr :: Int -> Text -> Text -> BL.ByteString
encodeErr c m r = encode $ ErrJSONRoot $ ErrJSON c m [Err m r]

s400 :: (FailureMsg fm, MonadError ServantErr m) => Text -> [fm] -> m a   
s400 = err 400

s401 :: (FailureMsg fm, MonadError ServantErr m) => Text -> [fm] -> m a   
s401 = err 401

s403 :: (FailureMsg fm, MonadError ServantErr m) => Text -> [fm] -> m a   
s403 = err 403

s404 :: (FailureMsg fm, MonadError ServantErr m) => Text -> [fm] -> m a   
s404 = err 404

s409 :: (FailureMsg fm, MonadError ServantErr m) => Text -> [fm] -> m a   
s409 = err 409

s500 :: (FailureMsg fm, MonadError ServantErr m) => Text -> [fm] -> m a   
s500 = err 500

s503 :: (FailureMsg fm, MonadError ServantErr m) => Text -> [fm] -> m a   
s503 = err 503
        
err :: (FailureMsg fm, MonadError ServantErr m) => Int -> Text -> [fm] -> m a   
err errCode msg errs = throwError $
  case errCode of
    400 -> resp err400
    401 -> resp err401
    403 -> resp err403
    404 -> resp err404 
    409 -> resp err409
    500 -> resp err500
    503 -> resp err503
    _   | errCode <= 499 -> resp err400
        | otherwise -> resp err500          
 where
   resp fn =           
     fn { errBody = encode (ErrJSONRoot $ ErrJSON errCode msg errs')
        , errHeaders = [contentType]
        }
   errs' = foldl' (\a e -> Err (failMsg e) (failReason e) : a) [] errs

contentType :: (HeaderName, ByteString)  
contentType = (hContentType, "application/json")  

-- Docs            

newtype Location = Location ByteString

instance ToByteString Location where
  builder (Location x) = builder x                    

instance ToSample Location where
  toSamples _ = singleSample $ Location ""
