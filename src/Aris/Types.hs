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
import Control.Applicative ((<|>))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.Default (Default(..))
import Control.Arrow (first)

--
-- Types
--

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
  show (AsStr x) = show x
instance (Read a) => Read (AsStr a) where
  readsPrec p s = map (first AsStr) $ readsPrec p s
  readList s = map (first $ map AsStr) $ readList s

newtype StrBool = StrBool { runStrBool :: Bool }
  deriving (Eq, Ord, Show, Read)
instance A.FromJSON StrBool where
  parseJSON (A.String "0") = return $ StrBool False
  parseJSON (A.String "1") = return $ StrBool True
  parseJSON _ = fail "expected bool as \"0\" or \"1\""
instance A.ToJSON StrBool where
  toJSON (StrBool b) = A.String $ if b then "1" else "0"

data Auth = Auth
  { a_user_id    :: Int
  , a_permission :: String
  , a_key        :: String
  } deriving (Eq, Ord, Show, Read)

data User = User
  { u_user_id      :: AsStr Int
  , u_user_name    :: String
  , u_display_name :: String
  , u_media_id     :: AsStr Int
  } deriving (Eq, Ord, Show, Read)

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
  { m_media_id  :: AsStr Int
  , m_game_id   :: AsStr Int
  , m_name      :: String
  , m_file_name :: String
  , m_url       :: String
  , m_thumb_url :: String
  } deriving (Eq, Ord, Show, Read)

ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2 } ''Auth
ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2 } ''User
ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2, ATH.omitNothingFields = True } ''Game
ATH.deriveJSON ATH.defaultOptions{ ATH.fieldLabelModifier = drop 2 } ''Media
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''GameType
ATH.deriveJSON ATH.defaultOptions{ ATH.constructorTagModifier = map toUpper } ''MapType
