{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module Aris.Base where

import Network.HTTP (simpleHTTP, postRequestWithBody, getResponseBody)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as ATH
import Data.Aeson ((.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<|>))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Text.Read (readMaybe)
import Data.Char (toUpper)

callAris :: (A.ToJSON a, A.FromJSON b) => String -> a -> IO (Return b)
callAris fun val = do
  rsp <- simpleHTTP $ postRequestWithBody
    ("http://dev.arisgames.org/server/json.php/v2." ++ fun)
    "application/x-www-form-urlencoded"
    (T.unpack $ TE.decodeUtf8 $ BL.toStrict $ A.encode val)
  body <- getResponseBody rsp
  case A.eitherDecodeStrict $ TE.encodeUtf8 $ T.pack body of
    Right x  -> return x
    Left err -> error $ "Aris.Base.callAris: invalid JSON returned by API; "
      ++ err ++ "; response body: " ++ body

data Return a
  = Fault { faultCode :: String, faultDetail :: String, faultString :: String }
  | Error { returnCode :: Int, returnCodeDescription :: String }
  | Data a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance (A.FromJSON a) => A.FromJSON (Return a) where
  parseJSON = A.withObject "return-code-wrapped object" $ \obj
    ->  do
      faultCode   <- obj .: "faultCode"
      faultDetail <- obj .: "faultDetail"
      faultString <- obj .: "faultString"
      return Fault{..}
    <|> do
      returnCode <- obj .: "returnCode"
      if returnCode == 0
        then fmap Data $ obj .: "data"
        else do
          returnCodeDescription <- obj .: "returnCodeDescription"
          return Error{..}

getGame :: Int -> IO (Return Game)
getGame i = callAris "games.getGame" $ A.object
  [ ("game_id", A.Number $ fromIntegral i)
  ]

newtype AsStr a = AsStr { runAsStr :: a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance (Read a) => A.FromJSON (AsStr a) where
  parseJSON = A.withText "value stored as string" $ \txt ->
    case readMaybe $ T.unpack txt of
      Nothing -> fail $ "couldn't read value from String: " ++ show txt
      Just x  -> return $ AsStr x

newtype StrBool = StrBool { runStrBool :: Bool }
  deriving (Eq, Ord, Show, Read)

instance A.FromJSON StrBool where
  parseJSON (A.String "0") = return $ StrBool False
  parseJSON (A.String "1") = return $ StrBool True
  parseJSON _ = fail "expected bool as \"0\" \"1\""

data Game = Game
  { g_game_id :: AsStr Int
  , g_name :: String
  , g_description :: String
  , g_icon_media_id :: AsStr Int
  , g_media_id :: AsStr Int
  , g_map_type :: MapType
  , g_map_latitude :: AsStr Double
  , g_map_longitude :: AsStr Double
  , g_map_zoom_level :: AsStr Double
  , g_map_show_player :: StrBool
  , g_map_show_players :: StrBool
  , g_map_offsite_mode :: StrBool
  , g_notebook_allow_comments :: StrBool
  , g_notebook_allow_likes :: StrBool
  , g_notebook_trigger_scene_id :: AsStr Int
  , g_notebook_trigger_requirement_root_package_id :: AsStr Int
  , g_notebook_trigger_title :: String
  , g_notebook_trigger_icon_media_id :: AsStr Int
  , g_notebook_trigger_distance :: AsStr Int
  , g_notebook_trigger_infinite_distance :: StrBool
  , g_notebook_trigger_wiggle :: StrBool
  , g_notebook_trigger_show_title :: StrBool
  , g_notebook_trigger_hidden :: StrBool
  , g_notebook_trigger_on_enter :: StrBool
  , g_inventory_weight_cap :: AsStr Int
  , g_published :: StrBool
  , g_type :: GameType
  , g_intro_scene_id :: AsStr Int
  } deriving (Eq, Ord, Show, Read)

data GameType
  = Location
  | Anywhere
  | QR
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data MapType
  = Street
  | Satellite
  | Hybrid
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

ATH.deriveFromJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2 } ''Game
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''GameType
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''MapType
