{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}

module Clicklac.API.Click
  ( ClickAPI
  , clickAPI
  ) where

import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS8       
import Data.Text (Text)
import qualified Data.Text as T       
import qualified Data.Text.Read as TR       
import Data.Int (Int64)
import Data.Maybe (fromJust)
       
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Aeson.TH.Extra (prefixRemovedOpts)
import Data.Time.Calendar (Day(..))        
import Data.Time.Clock (UTCTime(..))              
import Database.PostgreSQL.Simple.Lifted (PostgresClient(..))       
import qualified Database.PostgreSQL.Simple.Lifted as PS       
import Database.PostgreSQL.Simple (Only(..))       
import Database.PostgreSQL.Simple.FromRow (field, FromRow(..))
import Database.PostgreSQL.Simple.FromField (FromField(..))
import Database.PostgreSQL.Simple.ToField (ToField(..))
import qualified Data.UUID as UUID
import Servant 
import Servant.Docs      

import Clicklac.Application (App)
import Clicklac.Servant.Combinator
  ( AuthProtected
  , WithUserId
  )                     
import Clicklac.Servant.Response (Location(..))
import Clicklac.Types (UserId(..))

newtype ClickText = ClickText Text
  deriving (Show, Eq, FromField, ToField, Ord)

deriveJSON defaultOptions ''ClickText    

newtype ClickId = ClickId Int64
  deriving (Show, Eq, FromField, ToField, Ord)
  
deriveJSON defaultOptions ''ClickId
           
instance FromHttpApiData ClickId where         
  parseUrlPiece t =
    case TR.decimal t  of
      Right res -> Right . ClickId . fst $ res
      Left e -> Left $ T.pack e  
     
newtype LikeCount = LikeCount Int64
  deriving (Show, Eq, FromField, ToField, Ord)

deriveJSON defaultOptions ''LikeCount  
  
newtype FavoriteCount = FavoriteCount Int64
  deriving (Show, Eq, FromField, ToField, Ord)

deriveJSON defaultOptions ''FavoriteCount      

data ClickPost = ClickPost
  { clpText :: !ClickText } deriving (Show, Eq)

deriveJSON (prefixRemovedOpts 3) ''ClickPost  
           
data Click = Click
  { clClickId   :: !ClickId
  , clUserId    :: !UserId
  , clText      :: !ClickText
  , clLikeCount :: !LikeCount
  , clFavCount  :: !FavoriteCount
  , clPostedAt  :: !UTCTime
  } deriving (Show, Eq)      
             
instance FromRow Click where
  fromRow = Click
    <$> field  <*> field <*> field
    <*> field  <*> field <*> field

deriveJSON (prefixRemovedOpts 2) ''Click
          
type ClickAPI = "click" :> AuthProtected :> ClickAPI'           
type ClickAPI' =
    WithUserId :> ReqBody '[JSON] ClickPost :> PostCreated '[JSON]
    (Headers '[Header "Location" Location] Click)
  :<|>
    Capture "clickid" ClickId :> DeleteNoContent '[JSON] () 
  :<|>
    "user" :> Capture "userid" UserId :> Get '[JSON] [Click]
  :<|>
    ReqBody '[JSON] Click :> Capture "clickid" ClickId
    :> PutNoContent '[JSON] ()
 
clickAPI :: ServerT ClickAPI App       
clickAPI = createClick
      :<|> deleteClickById
      :<|> getClicksByUserId
      :<|> updateClick
          
getClicksByUserId :: PostgresClient m => UserId -> m [Click]
getClicksByUserId uid = 
  PS.query  " select id, user_id, click_text, like_count, \
            \ favorite_count, posted_at from click \
            \ where user_id = ? \
            \ order by posted_at desc"
             (Only uid)          
                
deleteClickById :: PostgresClient m => ClickId -> m ()
deleteClickById cid = 
  void $ PS.execute "delete from click where id =?" (Only cid)
  
createClick :: UserId
            -> ClickPost
            -> App (Headers '[Header "Location" Location] Click)
createClick uid ClickPost{..} = do
  [(cid, postedAt)] <- PS.query
    " insert into click(user_id, click_text, like_count, \
    \ favorite_count, posted_at) values(?,?,0,0,now()) returning id, posted_at"
    (uid, clpText)

  return $ addHeader (Location $ BS8.pack ("/click/" ++ show cid) )
    (Click (ClickId cid) uid clpText (LikeCount 0) (FavoriteCount 0) postedAt)

updateClick :: PostgresClient m => Click -> ClickId -> m ()
updateClick Click{..} cid =
  void $ PS.execute " update click set click_text =?, like_count =?, \
                    \ favorite_count =?  where id =?"
                   (clText, clLikeCount, clFavCount, cid)
  
-- Docs
  
instance ToCapture (Capture "clickid" ClickId) where
  toCapture _ =
    DocCapture "clickid" 
               "(integer) click id"

instance ToSample Click where
  toSamples _ = singleSample $ Click
      (ClickId 21)
      (UserId (fromJust (UUID.fromString "ccdbfee4-f7f8-4112-b050-dcc33ad4d6bf")))
      (ClickText "hi there")
      (LikeCount 21)
      (FavoriteCount 3)
      (UTCTime (ModifiedJulianDay 57670) 18000)


instance ToSample ClickPost where
  toSamples _ = singleSample $  ClickPost (ClickText "hi there")
