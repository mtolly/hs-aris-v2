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
  { gameID :: Int
  , gameName :: String
  , gameDescription :: String
  , iconMediaID :: Int
  , mediaID :: Int
  , gameType :: GameType
  , mapType :: MapType
  } deriving (Eq, Ord, Show, Read)

instance A.FromJSON Game where
  parseJSON = A.withObject "game object" $ \obj -> do
    gameID          <- parseRead $ obj .: "game_id"
    gameName        <-             obj .: "name"
    gameDescription <-             obj .: "description"
    iconMediaID     <- parseRead $ obj .: "icon_media_id"
    mediaID         <- parseRead $ obj .: "media_id"
    gameType        <-             obj .: "type"
    mapType         <-             obj .: "map_type"
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
