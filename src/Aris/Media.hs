{-# LANGUAGE OverloadedStrings #-}
module Aris.Media where

import qualified Data.Aeson as A

import Aris.Types
import Aris.Base

createMedia
  :: Media -- ^ {data, file_name, game_id, name?}
  -> Auth
  -> Aris Media
createMedia = callWithAuth "media.createMedia"

updateMedia
  :: Media -- ^ {media_id, name}
  -> Auth
  -> Aris Media
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
