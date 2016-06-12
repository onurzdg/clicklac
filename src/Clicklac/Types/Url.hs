
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Clicklac.Types.Url
  ( Url
  , urlT
  , validateUrl
  ) where

import qualified Data.Char as C
import qualified Data.List as L
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import Data.Aeson.Types (Value(String), typeMismatch)
import Database.CQL.Protocol
  ( Cql(..)
  , Tagged(..)
  , ColumnType (TextColumn)
  , Value(CqlText)
  )
import qualified Network.URI as NU

import Clicklac.Types.ValidationState
import Clicklac.Validation
  ( Validatable(..)
  , ValidationFailure (InvalidUrl)
  , success
  , failure
  )

newtype Url a = Url Text

urlT :: Url a -> Text
urlT (Url a) = a

deriving instance Show (Url n)
deriving instance Eq (Url n)

instance ToJSON (Url Validated) where
  toJSON (Url e) = String e

instance FromJSON (Url Unvalidated) where
  parseJSON (String u) = pure $ Url u
  parseJSON u = typeMismatch "Expected Url U" u

instance Cql (Url Validated) where
  ctype = Tagged TextColumn
  toCql (Url b) = CqlText b
  fromCql (CqlText b) = Right (Url b)
  fromCql _           = Left "Url V: Expected CqlText"

-- Not a robust validation function
validateUrl :: Text -> Maybe (Url Validated)
validateUrl (T.unpack -> urlT')
  | "http://" `L.isPrefixOf`  urlT' = parse urlT'
  | "https://" `L.isPrefixOf` urlT' = parse urlT'
  | "http" `L.isPrefixOf` urlT' = Nothing
  | isJust $ L.find (== ':') urlT' = Nothing
  | not . null $ L.takeWhile (not . C.isAlpha) urlT' = Nothing
  | otherwise = parse $ "http://" ++ urlT'
 where
   parse url' =
     NU.parseURI url' >>= \pUrl ->
      NU.uriAuthority pUrl >>= \auth ->
        if null (NU.uriUserInfo auth) &&
             (not . null) (NU.uriRegName auth) &&
             (not . L.isInfixOf "www" $ map C.toLower (NU.uriPath  pUrl))
         then return . Url $ T.pack url'
         else Nothing

instance Validatable (Maybe (Url Unvalidated)) where
  type Unvalidated' (Maybe (Url Unvalidated)) = Maybe (Url Validated)
  validate Nothing = success Nothing
  validate (Just (Url url')) =
    maybe (failure $ return InvalidUrl)
          (success . return)
          (validateUrl url')
