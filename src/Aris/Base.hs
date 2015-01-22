{-# LANGUAGE OverloadedStrings #-}
module Aris.Base where

import Network.HTTP (simpleHTTP, postRequestWithBody, getResponseBody)
import qualified Data.Aeson as A
import Data.Aeson ((.:))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

callAris :: String -> A.Value -> IO (Return A.Value)
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
  -- = Fault { faultCode :: String, faultDetail :: String, faultString :: String }
  = Error Int String
  | Data a
  deriving (Eq, Ord, Show, Read)

instance (A.FromJSON a) => A.FromJSON (Return a) where
  parseJSON = A.withObject "return-code-wrapped object" $ \obj -> do
    code <- obj .: "returnCode"
    if code == 0
      then fmap Data $ obj .: "data"
      else do
        desc <- obj .: "returnCodeDescription"
        return $ Error code desc

getGame :: Int -> IO (Return A.Value)
getGame i = callAris "games.getGame" $
  A.object [(T.pack "game_id", A.Number $ fromIntegral i)]
