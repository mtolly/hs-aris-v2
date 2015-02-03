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
import Data.Default (Default(..))
import qualified Data.HashMap.Strict as HM

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

data ArisError
  = Error { returnCode :: Int, returnCodeDescription :: String }
  | Fault { faultCode :: String, faultDetail :: String, faultString :: String }
  | Internal String
  deriving (Eq, Ord, Show, Read)

newtype ArisT m a = ArisT { runArisT :: ExceptT ArisError m a } deriving
  ( Eq, Ord, Show, Read
  , Functor, Foldable, Traversable, Applicative, Monad
  , MonadTrans, MonadIO
  )
type Aris = ArisT IO

arisIO :: Aris a -> IO a
arisIO act = runExceptT (runArisT act) >>= either (error . show) return

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

callWithAuth :: (A.ToJSON a, A.FromJSON b) => String -> a -> Auth -> Aris b
callWithAuth fun val auth = case A.toJSON val of
  A.Object obj -> callAris fun $ A.Object $ HM.insert "auth" (A.toJSON auth) obj
  notObject    -> ArisT $ throwE $ Internal $ unwords
    [ "Aris.Base.callWithAuth: couldn't call function"
    , fun
    , "because an authenticated value wasn't an object, but was instead"
    , show notObject
    ]

--
-- games.php
--

createGame :: Game -> Auth -> Aris Game
createGame = callWithAuth "games.createGame"

updateGame :: Game -> Auth -> Aris Game
updateGame = callWithAuth "games.updateGame"

getGame :: Int -> Aris Game
getGame i = callAris "games.getGame" $ A.object
  [ ("game_id", A.toJSON i)
  ]

getGamesForUser :: Auth -> Aris [Game]
getGamesForUser = callWithAuth "games.getGamesForUser" $ A.object []

deleteGame :: Int -> Auth -> Aris ()
deleteGame i = callWithAuth "games.deleteGame" $ A.object
  [ ("game_id", A.toJSON i)
  ]

-- TODO: getFullGame

--
-- users.php
--

-- TODO: createUser

-- TODO: updateUser

logIn :: String -> String -> Aris UserAuth
logIn un pw = callAris "users.logIn" $ A.object
  [ ("user_name", A.toJSON un)
  , ("password", A.toJSON pw)
  , ("permission", "read_write")
  ]

-- TODO: changePassword

-- TODO: fixPassword

getUser :: Int -> Auth -> Aris (Maybe User)
getUser i = callWithAuth "users.getUser" $ A.object
  [ ("user_id", A.toJSON i)
  ]

getUsersForGame :: Int -> Auth -> Aris [User]
getUsersForGame i = callWithAuth "users.getUsersForGame" $ A.object
  [ ("game_id", A.toJSON i)
  ]

-- TODO: getUsersForFuzzySearch

-- TODO: getUserForSearch

-- TODO: requestForgotPasswordEmail

--
-- media.php
--

getMedia :: Int -> Aris Media
getMedia i = callAris "media.getMedia" $ A.object
  [ ("media_id", A.toJSON i)
  ]

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
  readsPrec p s = map (\(x, y) -> (AsStr x, y)) $ readsPrec p s
  readList s = map (\(xs, y) -> (map AsStr xs, y)) $ readList s

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
