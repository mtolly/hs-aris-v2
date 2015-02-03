{-# LANGUAGE OverloadedStrings #-}
module Aris.Users where

import qualified Data.Aeson as A

import Aris.Types
import Aris.Base

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
