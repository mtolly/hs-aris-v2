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
import Data.Aeson.Types (Parser)
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

data Game = Game
  { gameID :: Int             -- $game->game_id                                      = $sql_game->game_id;
  , gameName :: String        -- $game->name                                         = $sql_game->name;
  , gameDescription :: String -- $game->description                                  = $sql_game->description;
  , iconMediaID :: Int        -- $game->icon_media_id                                = $sql_game->icon_media_id;
  , mediaID :: Int            -- $game->media_id                                     = $sql_game->media_id;
  , published :: Bool         -- $game->published                                    = $sql_game->published;
  , gameType :: GameType      -- $game->type                                         = $sql_game->type;
  , introSceneID :: Int       -- $game->intro_scene_id                               = $sql_game->intro_scene_id;
  , mapType :: MapType        -- $game->map_type                                     = $sql_game->map_type;
  , mapLatitude :: Double     -- $game->map_latitude                                 = $sql_game->map_latitude;
  , mapLongitude :: Double    -- $game->map_longitude                                = $sql_game->map_longitude;
  , mapZoomLevel :: Double    -- $game->map_zoom_level                               = $sql_game->map_zoom_level;
  , mapShowPlayer :: Bool     -- $game->map_show_player                              = $sql_game->map_show_player;
  , mapShowPlayers :: Bool    -- $game->map_show_players                             = $sql_game->map_show_players;
  , mapOffsiteMode :: Bool    -- $game->map_offsite_mode                             = $sql_game->map_offsite_mode;
  , notebookAllowComments :: Bool -- $game->notebook_allow_comments                      = $sql_game->notebook_allow_comments;
  , notebookAllowLikes :: Bool -- $game->notebook_allow_likes                         = $sql_game->notebook_allow_likes;
  -- $game->notebook_trigger_scene_id                    = $sql_game->notebook_trigger_scene_id;
  -- $game->notebook_trigger_requirement_root_package_id = $sql_game->notebook_trigger_requirement_root_package_id;
  -- $game->notebook_trigger_title                       = $sql_game->notebook_trigger_title;
  -- $game->notebook_trigger_icon_media_id               = $sql_game->notebook_trigger_icon_media_id;
  -- $game->notebook_trigger_distance                    = $sql_game->notebook_trigger_distance;
  -- $game->notebook_trigger_infinite_distance           = $sql_game->notebook_trigger_infinite_distance;
  -- $game->notebook_trigger_wiggle                      = $sql_game->notebook_trigger_wiggle;
  -- $game->notebook_trigger_show_title                  = $sql_game->notebook_trigger_show_title;
  -- $game->notebook_trigger_hidden                      = $sql_game->notebook_trigger_hidden;
  -- $game->notebook_trigger_on_enter                    = $sql_game->notebook_trigger_on_enter;
  , inventoryWeightCap :: Int -- $game->inventory_weight_cap                         = $sql_game->inventory_weight_cap;
  } deriving (Eq, Ord, Show, Read)

instance A.FromJSON Game where
  parseJSON = A.withObject "game object" $ \obj -> do
    gameID          <- parseRead $ obj .: "game_id"
    gameName        <-             obj .: "name"
    gameDescription <-             obj .: "description"
    iconMediaID     <- parseRead $ obj .: "icon_media_id"
    mediaID         <- parseRead $ obj .: "media_id"
    published       <- parseBool $ obj .: "published"
    gameType        <-             obj .: "type"
    introSceneID    <- parseRead $ obj .: "intro_scene_id"
    mapType         <-             obj .: "map_type"
    mapLatitude     <- parseRead $ obj .: "map_latitude"
    mapLongitude    <- parseRead $ obj .: "map_longitude"
    mapZoomLevel    <- parseRead $ obj .: "map_zoom_level"
    mapShowPlayer   <- parseBool $ obj .: "map_show_player"
    mapShowPlayers  <- parseBool $ obj .: "map_show_players"
    mapOffsiteMode  <- parseBool $ obj .: "map_offsite_mode"
    notebookAllowComments <- parseBool $ obj .: "notebook_allow_comments"
    notebookAllowLikes    <- parseBool $ obj .: "notebook_allow_likes"
    inventoryWeightCap    <- parseRead $ obj .: "inventory_weight_cap"
    return Game{..}

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

parseBool :: Parser String -> Parser Bool
parseBool p = do
  s <- p
  case s of
    "0" -> return False
    "1" -> return True
    _   -> fail $ "couldn't read Bool from String: " ++ show s

parseRead :: (Read a) => Parser String -> Parser a
parseRead p = do
  s <- p
  case readMaybe s of
    Just x -> return x
    Nothing -> fail $ "couldn't read value from String: " ++ show s

ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''GameType
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''MapType
