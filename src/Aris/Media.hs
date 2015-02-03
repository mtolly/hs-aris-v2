{-# LANGUAGE OverloadedStrings #-}
module Aris.Media where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as B8

import Aris.Types
import Aris.Base

createMediaBS
  :: B.ByteString
  -> Media -- ^ {file_name, game_id, name?}
  -> Auth
  -> Aris Media
createMediaBS = createMedia . B8.unpack . B64.encode

createMedia
  :: String -- ^ base64 data
  -> Media -- ^ {file_name, game_id, name?}
  -> Auth
  -> Aris Media
createMedia b64 media = case A.toJSON media of
  A.Object mobj ->
    callWithAuth "media.createMedia" $ HM.insert "data" (A.toJSON b64) mobj
  notObject -> fail $
    "createMedia: couldn't insert media data into non-object " ++ show notObject

updateMedia :: Media -> Auth -> Aris Media
updateMedia = callWithAuth "media.updateMedia"

getMedia :: Int -> Aris Media
getMedia i = callAris "media.getMedia" $ A.object
  [ ("media_id", A.toJSON i)
  ]

getMediaForGame :: Int -> Aris [Media]
getMediaForGame i = callAris "media.getMediaForGame" $ A.object
  [ ("game_id", A.toJSON i)
  ]

deleteMedia :: Int -> Auth -> Aris ()
deleteMedia i = callWithAuth "media.deleteMedia" $ A.object
  [ ("media_id", A.toJSON i)
  ]
