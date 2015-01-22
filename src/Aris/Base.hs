module Aris.Base where

import Network.HTTP (simpleHTTP, postRequestWithBody, getResponseBody)
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust)

callAris :: String -> A.Value -> IO A.Value
callAris fun val = do
  rsp <- simpleHTTP $ postRequestWithBody
    ("http://dev.arisgames.org/server/json.php/v2." ++ fun)
    "application/x-www-form-urlencoded"
    (T.unpack $ TE.decodeUtf8 $ BL.toStrict $ A.encode val)
  fmap (fromJust . A.decodeStrict' . TE.encodeUtf8 . T.pack) $ getResponseBody rsp

getGame :: Int -> IO A.Value
getGame i = callAris "games.getGame" $
  A.object [(T.pack "game_id", A.Number $ fromIntegral i)]
