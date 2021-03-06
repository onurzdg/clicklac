{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeOperators      #-}

module Clicklac.API where

import Control.Monad.Trans.Except
import Data.Char (isSpace)
import Data.Bool (bool)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable.Internal

import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status
  ( statusMessage
  , status200
  , status404
  )
import Network.Wai
  ( Application
  , Response
  , ResponseReceived
  , responseLBS
  , responseFile
  )
import Servant hiding (serve, toApplication)
import Servant.Server.Internal hiding (toApplication, responseServantErr)
import System.Logger (Logger)
import qualified System.Logger as L

import Clicklac.Application (App, runApp, AppConfig(..))
import Clicklac.API.Authentication (authAPI, AuthAPI)
import Clicklac.API.Click (clickAPI, ClickAPI)
import Clicklac.API.User (userAPI, UserAPI)
import Clicklac.OpFailure (FailureMsg(..))
import Clicklac.Servant.Response (RequestFailure(..))
import qualified Clicklac.Servant.Response as RS

---------- API Config --------------------------
type API'' = (AuthAPI :<|> UserAPI :<|> ClickAPI)
type APIv1 = "api" :> "v1" :> API''
type APIv0 = "api" :> API'' -- points to latest API version
type API = APIv1 :<|> APIv0
type APIRaw = "file" :> Raw :<|> "users" :> "file" :> Raw 
type API' = API :<|> APIRaw

apiV1 :: AppConfig -> ServerT APIv1 App
apiV1 _ = authAPI :<|> userAPI :<|> clickAPI

-- Always points to the latest version
apiV0 :: AppConfig -> ServerT APIv0 App
apiV0 = apiV1

rawAPIs :: AppConfig -> Server APIRaw
rawAPIs cf = directory cf :<|> myfile cf

-- File handler examples
myfile :: AppConfig -> Server Raw
myfile _ _req send =
  send $ responseFile status200 [(hContentType, "text/html")]
                                "index.html"
                                 Nothing

directory :: AppConfig -> Server Raw
directory _ _req send =
  send $ responseLBS status404 [RS.contentType] $
         RS.encodeErr' 404 ResourceNotFound

---------- Servant Config ----------------------------
appServerT :: AppConfig -> ServerT API App
appServerT cfg = apiV1 cfg :<|> apiV0 cfg

appServer :: AppConfig -> Server API
appServer cfg = enter appToExcept $ appServerT cfg
 where appToExcept :: App :~> ExceptT ServantErr IO
       appToExcept = Nat $ \a -> runApp a cfg

app :: AppConfig -> Application
app conf = serve (Proxy :: Proxy API')
                 (conf :. EmptyContext)
                 (appServer conf :<|> rawAPIs conf)

-- Modified versions of original functions

serve :: (HasServer layout context, HasContextEntry context AppConfig)
      => Proxy layout -> Context context -> Server layout -> Application
serve p context server =
  toApplication (logger cf) (runRouter (route p context d))
 where
   d = Delayed r r r r (\ _ _ _ -> Route server)
   r = return (Route ())
   cf = getContextEntry context :: AppConfig

toApplication :: Logger -> RoutingApplication -> Application
toApplication lg ra req respond = ra req routingRespond
 where
   routingRespond :: RouteResult Response -> IO ResponseReceived
   routingRespond (Fail err) = do
     L.err lg $ L.msg $ show err
     respond $ responseServantErr err
   routingRespond (FailFatal err) = do
     L.err lg $ L.msg $ show err
     respond $ responseServantErr err
   routingRespond (Route v) = respond v

responseServantErr :: ServantErr -> Response
responseServantErr ServantErr{..} =
  responseLBS status (RS.contentType : errHeaders) encodedBody
 where
   status = HTTP.mkStatus errHTTPCode (UTF8.fromString errReasonPhrase)
   encodedBody =
     case errHTTPCode of
       400 ->
         let errBody' = LBS.toStrict errBody
         in RS.encodeErr 400 (T.pack "Invalid request body: " `T.append`
              TE.decodeUtf8 (fromMaybe errBody'
                (asum $ map ($ errBody')
                  [stripMissingKeyDetails, stripBadJsonDetails])))
              (failReason InvalidRequestBody)
       404 -> RS.encodeErr' 404 UrlNotFound
       405 -> RS.encodeErr' 405 MethodNotAllowed
       415 -> RS.encodeErr' 415 UnsupportedMedia
       -- | When an exception occurs, the callback passed to Warp's
       --   setOnExceptionResponse will be called. Probably, this error
       --   condition here will not be executed ever.
       500 -> RS.encodeErr' 500 InternalError
       _ -> RS.encodeErr errHTTPCode bodyOrStatusMsg $
                 T.filter (not . isSpace) (T.pack errReasonPhrase)

   bodyOrStatusMsg = TL.toStrict . TLE.decodeUtf8 $
                       bool (LBS.fromStrict $ statusMessage status)
                       errBody
                       (not . LBS.null $ errBody)

    -- | Functions that strip out non-essentail details in Aeson error messages.
    --   They are prone to break with new major releases of Aeson.

   stripMissingKeyDetails errBody' =
     let lsub = snd $ BS.breakSubstring (UTF8.fromString "key") errBody'
     in if BS.null lsub then Nothing else Just ("K" `BS.append` BS.tail lsub)

   stripBadJsonDetails errBody' =
     let lsub = snd $ BS.breakSubstring (UTF8.fromString "Failed reading:") errBody'
     in if BS.null lsub then Nothing else Just (UTF8.fromString "Bad JSON")
