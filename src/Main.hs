{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
        
module Main where 

import Control.Exception (SomeException, catch)      
import Control.Exception.Base (fromException)
import Control.Monad (void, when)
import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)       
import System.Exit (die)
       
import qualified Crypto.Nonce as N
import Data.Default.Class (def)
import qualified Data.Configurator as CFG
import Data.Configurator.Types
import qualified Database.CQL.IO as CS
import qualified Database.CQL.Protocol as CP       
import Database.PostgreSQL.Simple.Pool (PoolConfig (..))
import qualified Database.PostgreSQL.Simple.Pool as PSP
import qualified Data.Vault.Lazy as VT
import Network.Wai.Handler.Warp (InvalidRequest)       
import qualified Network.Wai.Handler.Warp as WP
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai (Response, Request, responseLBS)       
import Network.Wai.Middleware.RequestLogger
  ( RequestLoggerSettings(..)
  , Destination(Logger)
  , OutputFormat(Apache)
  )
import Network.HTTP.Types.Status 
  ( badRequest400
  , internalServerError500
  )  
import qualified Network.Wai.Middleware.RequestLogger as RL

import System.FilePath       
import System.Logger (Level (Info, Error), Logger)
import qualified System.Logger as L       
import System.Log.FastLogger (newFileLoggerSet, rmLoggerSet)
import qualified System.Posix.Signals as SG 
import System.Posix.Signals (Handler (CatchOnce))              
import qualified Web.ClientSession as WCS
       
import Clicklac.API (app)
import Clicklac.Application (AppConfig(..))
import Clicklac.Environment
  ( lookupAppEnv'
  , AppEnvironment(..)
  )        
import Clicklac.Servant.Response (RequestFailure(BadRequest, InternalError))
import qualified Clicklac.Servant.Response as RS
import Clicklac.Middleware.AuthCheck (authCheck)

main :: IO ()
main = do     
  appEnv <- lookupAppEnv'
  
  cfg <- CFG.load $ configFile "app.cfg"
  -- set up server logger
  sLogger <- L.new
            . L.setLogLevel L.Info 
            . L.setOutput (L.Path "log/server.log")
            . L.setName  (Just $ T.pack "s")
            $ L.defSettings
  let logFlushClose = L.close sLogger 
      logI = L.log sLogger Info . L.msg . UTF8.fromString
      logE = L.log sLogger Error . L.msg . UTF8.fromString

  let terminateEarly errMsg = logE errMsg >>
                              logFlushClose >>
                              die errMsg
  
  createDirectoryIfMissing False "log" 

  logI "--- Booting ---"   
    
  logI "--- Initing CS driver ---"
  let settings' = CS.setKeyspace (CP.Keyspace $ T.pack "clicklac")
                . CS.setRetrySettings (CS.constDelay 5 CS.noRetry)
                $ CS.setPolicy CS.roundRobin CS.defSettings    
  csClient <- CS.init (L.clone (Just $ T.pack "c") sLogger) settings' `catch`
                      (\(err :: SomeException) -> do
                      let errMsg = "Cassandra client => " ++ show err
                      terminateEarly errMsg)
    
  logI "--- Creating Nonce generator ---"
  nonceG <- N.new

  -- key used in generation of cookie encryption
  sessKey <- WCS.getKey "client_session_key.aes"

  -- key used in middleware and retrieval of UserId in API combinator
  vKey <- VT.newKey 

  -- set-up apache style access file logger for warp
  warpLoggerSet <- newFileLoggerSet 4096 "log/access.log"
  reqLgr <- RL.mkRequestLogger $ 
              def { outputFormat = bool (Apache RL.FromSocket)
                                        (RL.Detailed True)
                                        (appEnv == Development)
                  , destination = Logger warpLoggerSet
                  }

  connStr <- loadPsgConnStr cfg                                       
  connPool' <- PSP.createConnPool $
    PoolConfig connStr 2 (60 * 10) 20        
  -- shutdown hook to execute when server closes
  let shutdownHook closeSock = 
        CS.shutdown csClient >>
        PSP.destroyConnPool connPool' >> -- shuts down DB conn pool
        logI "--- Shutting down server ---" >>
        logFlushClose >>
        rmLoggerSet warpLoggerSet >>
        closeSock -- terminates server
    
  logI "--- Starting Warp server ---"

  WP.runSettings (
    WP.setInstallShutdownHandler (\c -> do
      void $ SG.installHandler SG.sigTERM (CatchOnce $ shutdownHook c) Nothing
      void $ SG.installHandler SG.sigINT  (CatchOnce $ shutdownHook c) Nothing
    )
    . WP.setOnExceptionResponse onExceptionResponse
    . WP.setOnException (onException sLogger)
    . WP.setServerName "" -- hide server name
    . WP.setPort 8081 $ WP.defaultSettings)
    -- Middlewares
    . authCheck csClient vKey
    . gzip def
    . reqLgr
    $ app (AppConfig csClient sLogger nonceG sessKey vKey connPool')

onExceptionResponse :: SomeException -> Response
onExceptionResponse e
  | Just (_ :: InvalidRequest) <- fromException e =
      responseLBS badRequest400 [RS.contentType] $
        RS.encodeErr' 400 BadRequest
  
  | otherwise = responseLBS internalServerError500 [RS.contentType] $
                  RS.encodeErr' 500 InternalError
                                   

onException :: Logger -> Maybe Request -> SomeException -> IO ()
onException l _ e = 
  when (WP.defaultShouldDisplayException e) 
        $ L.log l Error . L.msg $ T.pack $ show e            
        
configFile :: FilePath -> [Worth FilePath]
configFile name = [Required $ "resources" </> name]

loadPsgConnStr :: Config -> IO ByteString
loadPsgConnStr cfg =
  psgConnStr
   <$> require' "psg.host"
   <*> require' "psg.dbname"
   <*> require' "psg.user"
   <*> require' "psg.password"
   <*> require' "psg.port"
   <*> require' "psg.connection_timeout"
 where
   require' = CFG.require cfg
   
   psgConnStr host dbName uname pass port timeout = BS.intercalate " " params
    where
      params =
        ["host=" <> host, "dbname=" <> dbName,
         "user=" <> uname, "password=" <> pass,
         "port=" <> port,  "connect_timeout=" <> timeout]          
