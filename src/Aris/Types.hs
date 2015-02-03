{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Aris.Types where

import qualified Data.Aeson as A
import qualified Data.Aeson.TH as ATH
import Data.Aeson ((.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Applicative ((<|>))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.Default (Default(..))
import Control.Arrow (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

-- | Wrapper for values which also allows deserializing from a string
-- using their "Read" instance.
newtype AsStr a = AsStr { runAsStr :: a } deriving
  ( Eq, Ord
  , Functor, Foldable, Traversable
  , Num, Enum, Integral, Real, Fractional, RealFrac
  )
instance (Read a, A.FromJSON a) => A.FromJSON (AsStr a) where
  parseJSON val
    =   do
      flip (A.withText "value stored as string") val $ \txt ->
        case readMaybe $ T.unpack txt of
          Nothing -> fail $ "couldn't read value from String: " ++ show txt
          Just x  -> return $ AsStr x
    <|> do
      fmap AsStr $ A.parseJSON val
instance (A.ToJSON a) => A.ToJSON (AsStr a) where
  toJSON (AsStr x) = A.toJSON x
instance (Show a) => Show (AsStr a) where
  showsPrec p (AsStr x) = showsPrec p x
instance (Read a) => Read (AsStr a) where
  readsPrec p s = map (first AsStr) $ readsPrec p s

newtype StrBool = StrBool { runStrBool :: Bool }
  deriving (Eq, Ord)
instance A.FromJSON StrBool where
  parseJSON (A.String "0") = return $ StrBool False
  parseJSON (A.String "1") = return $ StrBool True
  parseJSON _ = fail "expected bool as \"0\" or \"1\""
instance A.ToJSON StrBool where
  toJSON (StrBool b) = A.String $ if b then "1" else "0"
instance Show StrBool where
  showsPrec p (StrBool x) = showsPrec p x
instance Read StrBool where
  readsPrec p s = map (first StrBool) $ readsPrec p s

newtype Base64 = Base64 { runBase64 :: B.ByteString }
  deriving (Eq, Ord)
instance A.ToJSON Base64 where
  toJSON (Base64 bs) = A.toJSON $ TE.decodeUtf8 $ B64.encode bs
instance A.FromJSON Base64 where
  parseJSON = A.withText "base64 string" $ \txt ->
    case B64.decode $ TE.encodeUtf8 txt of
      Left err -> fail $ "could not decode base64 string from JSON: " ++ err
      Right bs -> return $ Base64 bs
instance Show Base64 where
  showsPrec p (Base64 x) = showsPrec p x
instance Read Base64 where
  readsPrec p s = map (first Base64) $ readsPrec p s

data Auth = Auth
  { a_user_id    :: Int
  , a_permission :: String
  , a_key        :: String
  } deriving (Eq, Ord, Show, Read)

data User = User
  { u_user_id      :: Maybe (AsStr Int)
  , u_user_name    :: Maybe String
  , u_password     :: Maybe String
  , u_email        :: Maybe String
  , u_display_name :: Maybe String
  , u_media_id     :: Maybe (AsStr Int)
  } deriving (Eq, Ord, Show, Read)
instance Default User where
  def = User def def def def def def

data UserAuth = UserAuth { ua_user :: User, ua_auth :: Auth }
  deriving (Eq, Ord, Show, Read)
instance A.FromJSON UserAuth where
  parseJSON v = do
    user <- A.parseJSON v
    case u_user_id user of
      Just (AsStr uid) -> flip (A.withObject "object with auth key") v $ \obj -> let
        authFor perm = do
          key <- obj .: T.pack (perm ++ "_key")
          return Auth
            { a_user_id    = uid
            , a_permission = perm
            , a_key        = key
            }
        in do
          auth <- authFor "read_write" <|> authFor "read" <|> authFor "write"
          return $ UserAuth user auth
      Nothing -> fail "couldn't read UserAuth because no user_id"

data Game = Game
  { g_game_id                                      :: Maybe (AsStr Int)
  , g_name                                         :: Maybe String
  , g_description                                  :: Maybe String
  , g_icon_media_id                                :: Maybe (AsStr Int)
  , g_media_id                                     :: Maybe (AsStr Int)
  , g_map_type                                     :: Maybe MapType
  , g_map_latitude                                 :: Maybe (AsStr Double)
  , g_map_longitude                                :: Maybe (AsStr Double)
  , g_map_zoom_level                               :: Maybe (AsStr Double)
  , g_map_show_player                              :: Maybe StrBool
  , g_map_show_players                             :: Maybe StrBool
  , g_map_offsite_mode                             :: Maybe StrBool
  , g_notebook_allow_comments                      :: Maybe StrBool
  , g_notebook_allow_likes                         :: Maybe StrBool
  , g_notebook_trigger_scene_id                    :: Maybe (AsStr Int)
  , g_notebook_trigger_requirement_root_package_id :: Maybe (AsStr Int)
  , g_notebook_trigger_title                       :: Maybe String
  , g_notebook_trigger_icon_media_id               :: Maybe (AsStr Int)
  , g_notebook_trigger_distance                    :: Maybe (AsStr Int)
  , g_notebook_trigger_infinite_distance           :: Maybe StrBool
  , g_notebook_trigger_wiggle                      :: Maybe StrBool
  , g_notebook_trigger_show_title                  :: Maybe StrBool
  , g_notebook_trigger_hidden                      :: Maybe StrBool
  , g_notebook_trigger_on_enter                    :: Maybe StrBool
  , g_inventory_weight_cap                         :: Maybe (AsStr Int)
  , g_published                                    :: Maybe StrBool
  , g_type                                         :: Maybe GameType
  , g_intro_scene_id                               :: Maybe (AsStr Int)
  } deriving (Eq, Ord, Show, Read)
instance Default Game where
  def = Game
    def def def def def def def
    def def def def def def def
    def def def def def def def
    def def def def def def def

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

data Media = Media
  { m_media_id  :: Maybe (AsStr Int)
  , m_game_id   :: Maybe (AsStr Int)
  , m_name      :: Maybe String
  , m_file_name :: Maybe String
  , m_url       :: Maybe String
  , m_thumb_url :: Maybe String
  , m_data      :: Maybe Base64
  } deriving (Eq, Ord, Show, Read)
instance Default Media where
  def = Media def def def def def def def

ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2 } ''Auth
ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2, ATH.omitNothingFields = True } ''User
ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2, ATH.omitNothingFields = True } ''Game
ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2, ATH.omitNothingFields = True } ''Media
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''GameType
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''MapType
