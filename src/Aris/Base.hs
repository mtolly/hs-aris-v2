{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Aris.Base where

import Network.HTTP (simpleHTTP, postRequestWithBody, getResponseBody)
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as ATH
import Data.Aeson ((.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<|>), Applicative(..))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Text.Read (readMaybe)
import Data.Char (toUpper)

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data ArisError
  = Error { returnCode :: Int, returnCodeDescription :: String }
  | Fault { faultCode :: String, faultDetail :: String, faultString :: String }
  | Internal String
  deriving (Eq, Ord, Show, Read)

arisIO :: Aris a -> IO a
arisIO act = runExceptT (runArisT act) >>= either (error . show) return

newtype ArisT m a = ArisT { runArisT :: ExceptT ArisError m a } deriving
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable, Applicative, Monad
  , MonadTrans, MonadIO
  )
type Aris = ArisT IO

instance (A.FromJSON a, Monad m) => A.FromJSON (ArisT m a) where
  parseJSON = A.withObject "return-code-wrapped object" $ \obj
    ->  do
      faultCode   <- obj .: "faultCode"
      faultDetail <- obj .: "faultDetail"
      faultString <- obj .: "faultString"
      return $ ArisT $ throwE Fault{..}
    <|> do
      returnCode <- obj .: "returnCode"
      if returnCode == 0
        then fmap return $ obj .: "data"
        else do
          returnCodeDescription <- obj .: "returnCodeDescription"
          return $ ArisT $ throwE Error{..}

callAris :: (A.ToJSON a, A.FromJSON b) => String -> a -> Aris b
callAris fun val = do
  rsp <- liftIO $ simpleHTTP $ postRequestWithBody
    ("http://dev.arisgames.org/server/json.php/v2." ++ fun)
    "application/x-www-form-urlencoded"
    (T.unpack $ TE.decodeUtf8 $ BL.toStrict $ A.encode val)
  body <- liftIO $ getResponseBody rsp
  case A.eitherDecodeStrict $ TE.encodeUtf8 $ T.pack body of
    Right x  -> x
    Left err -> ArisT $ throwE $ Internal
      $ "Aris.Base.callAris: invalid JSON returned by API; "
      ++ err ++ "; response body: " ++ body

getGame :: Int -> Aris Game
getGame i = callAris "games.getGame" $ A.object
  [ ("game_id", A.toJSON i)
  ]

data Auth = Auth
  { a_user_id    :: Int
  , a_permission :: String
  , a_key        :: String
  } deriving (Eq, Ord, Show, Read)

data User = User
  { u_user_id :: AsStr Int
  , u_user_name :: String
  , u_display_name :: String
  , u_media_id :: AsStr Int
  } deriving (Eq, Ord, Show, Read)

getGamesForUser :: Auth -> Aris [Game]
getGamesForUser auth = callAris "games.getGamesForUser" $ A.object
  [ ("auth", A.toJSON auth)
  ]

data UserAuth = UserAuth { ua_user :: User, ua_auth :: Auth }
  deriving (Eq, Ord, Show, Read)

instance A.FromJSON UserAuth where
  parseJSON v = do
    user <- A.parseJSON v
    flip (A.withObject "object with auth key") v $ \obj -> do
      let authFor perm = do
            key <- obj .: T.pack (perm ++ "_key")
            return Auth
              { a_user_id    = runAsStr $ u_user_id user
              , a_permission = perm
              , a_key        = key
              }
      auth <- authFor "read_write" <|> authFor "read" <|> authFor "write"
      return $ UserAuth user auth

logIn :: String -> String -> Aris UserAuth
logIn un pw = callAris "users.logIn" $ A.object
    [ ("user_name", A.toJSON un)
    , ("password", A.toJSON pw)
    , ("permission", "read_write")
    ]

getUser :: Auth -> Int -> Aris (Maybe User)
getUser auth i = callAris "users.getUser" $ A.object
  [ ("auth", A.toJSON auth)
  , ("user_id", A.toJSON i)
  ]

newtype AsStr a = AsStr { runAsStr :: a } deriving
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable
  , Num, Enum, Integral, Real, Fractional, RealFrac
  )

instance (Read a) => A.FromJSON (AsStr a) where
  parseJSON = A.withText "value stored as string" $ \txt ->
    case readMaybe $ T.unpack txt of
      Nothing -> fail $ "couldn't read value from String: " ++ show txt
      Just x  -> return $ AsStr x

instance (Show a) => A.ToJSON (AsStr a) where
  toJSON (AsStr x) = A.String $ T.pack $ show x

newtype StrBool = StrBool { runStrBool :: Bool }
  deriving (Eq, Ord, Show, Read)

instance A.FromJSON StrBool where
  parseJSON (A.String "0") = return $ StrBool False
  parseJSON (A.String "1") = return $ StrBool True
  parseJSON _ = fail "expected bool as \"0\" \"1\""

instance A.ToJSON StrBool where
  toJSON (StrBool b) = A.String $ if b then "1" else "0"

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

ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2 } ''Auth
ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2, ATH.omitNothingFields = True } ''User

ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2 } ''Game
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''GameType
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''MapType
