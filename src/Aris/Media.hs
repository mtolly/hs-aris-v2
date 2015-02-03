{-# LANGUAGE OverloadedStrings #-}
module Aris.Media where

import qualified Data.Aeson as A

import Aris.Types
import Aris.Base

--
-- media.php
--

getMedia :: Int -> Aris Media
getMedia i = callAris "media.getMedia" $ A.object
  [ ("media_id", A.toJSON i)
  ]
