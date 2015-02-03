{-# LANGUAGE OverloadedStrings #-}
module Aris.Games where

import qualified Data.Aeson as A

import Aris.Types
import Aris.Base

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
