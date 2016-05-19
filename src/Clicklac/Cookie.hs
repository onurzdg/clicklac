
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Clicklac.Cookie
  ( -- * Encode/Decode cookie contents for client side storage
    encodeCookieContents
  , decodeCookieContents

    -- * StorageCookie
  , StorageCookie
  , scUserId
  , storageCookie
  , sessionIdCookieKey
  , sessionStoreCookieKey

    -- * Cookie creation/expiration
  , cookieHeaderBS
  , cookieHeader
  , expiredCookieHeaderBS
  , expiredCookieHeader
  , CookieHeader
  ) where

import Data.ByteString.Char8 as BS8
import Data.ByteString.Conversion.To (ToByteString (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import Blaze.ByteString.Builder as B
import Data.Composition ((.**))
import Data.Serialize (Serialize)
import qualified Data.Serialize as Ser
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..))
import Servant (FromHttpApiData (..))
import Web.Cookie

import Clicklac.OpFailure (FailureMsg (..))
import Clicklac.Types (CookieEncryption (..), UserId)

type ExpirationDate = UTCTime
type CookieName = ByteString
type CookieVal = ByteString

data CookieOpFailure
  = CorruptedCookie
  | CookieDecodeFailure String
  | CookieDecryptFailure
  | StorageCookieNotFound
  | SessIdCookieNotFound
  deriving (Show, Eq)

instance FailureMsg CookieOpFailure where
  failMsg CorruptedCookie           = "Cookie is corrupted"
  failMsg (CookieDecodeFailure str) = T.pack str
  failMsg CookieDecryptFailure      = "Failed to decrypt cookie"
  failMsg StorageCookieNotFound     = "Storage cookie not found"
  failMsg SessIdCookieNotFound      = "SessinId cookie not found"

data StorageCookie = SC {scUserId :: UserId}
  deriving (Show, Generic)

storageCookie :: UserId -> StorageCookie
storageCookie = SC

instance Serialize StorageCookie

newtype CookieHeader = CookieHeader ByteString

instance ToByteString CookieHeader where
  builder (CookieHeader x) = builder x

instance FromHttpApiData CookieHeader where
  parseUrlPiece = Right . CookieHeader . TE.encodeUtf8

encodeCookieContents :: (Serialize s, CookieEncryption m)
                     => s
                     -> m ByteString
encodeCookieContents = encryptCookie . Ser.encode

decodeCookieContents :: (Serialize s, CookieEncryption m)
                     => ByteString
                     -> m (Either CookieOpFailure s)
decodeCookieContents content = do
  decm <- decryptCookie content
  return $ case decm of
             Just dec -> decode dec
             Nothing  -> Left CookieDecryptFailure
 where
   decode bs =
     either (Left . CookieDecodeFailure)
            Right
            (Ser.decode bs)

cookieHeader :: CookieName -> CookieVal -> ExpirationDate -> CookieHeader
cookieHeader = CookieHeader .** cookieHeaderBS

cookieHeaderBS :: CookieName -> CookieVal -> ExpirationDate -> ByteString
cookieHeaderBS name val expTime =
  B.toByteString . renderSetCookie $
    def { setCookieName = name
        , setCookieValue = val
        , setCookieExpires =  Just expTime
        , setCookieHttpOnly = True
        }

expiredCookieHeader :: CookieName -> CookieHeader
expiredCookieHeader = CookieHeader . expiredCookieHeaderBS

expiredCookieHeaderBS :: CookieName -> ByteString
expiredCookieHeaderBS name =
  B.toByteString . renderSetCookie $
    def { setCookieName = name
        , setCookieValue = ""
        , setCookieExpires =  Just $ UTCTime (ModifiedJulianDay 0) 0
        , setCookieHttpOnly = True
        }

sessionIdCookieKey :: CookieName
sessionIdCookieKey = BS8.pack "_sid"

sessionStoreCookieKey :: CookieName
sessionStoreCookieKey = BS8.pack "_ssid"
