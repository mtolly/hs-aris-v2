{-# LANGUAGE OverloadedStrings #-}
module Aris.Users where

import qualified Data.Aeson as A

import Aris.Types
import Aris.Base

createUser
  :: User -- ^ {user_name, password, email?, media_id?, display_name?}
  -> Aris UserAuth
createUser = callAris "users.createUser"

updateUser
  :: User -- ^ {display_name?, email?, media[_id]?}
  -> Auth
  -> Aris UserAuth
updateUser = callWithAuth "users.updateUser"

logIn
  :: String -- ^ username
  -> String -- ^ password
  -> Aris UserAuth
logIn un pw = callAris "users.logIn" $ A.object
  [ ("user_name" , A.toJSON un )
  , ("password"  , A.toJSON pw )
  , ("permission", "read_write")
  ]

logInCall
  :: String -- ^ username
  -> String -- ^ password
  -> (Auth -> Aris a)
  -> Aris a
logInCall un pw f = logIn un pw >>= f . ua_auth

changePassword
  :: String -- ^ username
  -> String -- ^ old password
  -> String -- ^ new password
  -> Aris UserAuth
changePassword un old new = callAris "users.changePassword" $ A.object
  [ ("user_name"   , A.toJSON un )
  , ("old_password", A.toJSON old)
  , ("new_password", A.toJSON new)
  ]

-- TODO: fixPassword

getUser :: Int -> Auth -> Aris (Maybe User)
getUser i = callWithAuth "users.getUser" $ A.object
  [ ("user_id", A.toJSON i)
  ]

getUsersForGame :: Int -> Auth -> Aris [User]
getUsersForGame i = callWithAuth "users.getUsersForGame" $ A.object
  [ ("game_id", A.toJSON i)
  ]

getUsersForFuzzySearch :: String -> Auth -> Aris [User]
getUsersForFuzzySearch s = callWithAuth "users.getUsersForFuzzySearch" $ A.object
  [ ("search", A.toJSON s)
  ]

getUserForSearch :: String -> Auth -> Aris User
getUserForSearch s = callWithAuth "users.getUserForSearch" $ A.object
  [ ("search", A.toJSON s)
  ]

requestForgotPasswordEmail
  :: Either String String -- ^ Left username or Right email
  -> Aris ()
requestForgotPasswordEmail x = callAris "users.requestForgotPasswordEmail" $ A.object
  [ case x of
    Left  username -> ("username", A.toJSON username)
    Right email    -> ("email"   , A.toJSON email   )
  ]
