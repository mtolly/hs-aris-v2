{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Aris.Base where

import Network.HTTP (simpleHTTP, postRequestWithBody, getResponseBody)
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Control.Applicative ((<|>), Applicative(..))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import qualified Data.HashMap.Strict as HM

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Aris.Types

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
