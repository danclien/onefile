#!/usr/bin/env stack
{- stack
  script
  --resolver nightly-2018-01-04
  --ghc-options -Wall
-}

{-

Based on the first Servant tutorial

* Adds a newtype for 'UserId'
* Adds 'UserId' to 'User'
* Adds an endpoint to get a 'User' by "UserId'
* Adds an endpoint to get multiple 'User's by comma-separated
  'UserId's

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

import Prelude ()
import Prelude.Compat

import Data.Aeson.Compat
import Data.Maybe
import Data.Text hiding (head)
import Data.Time.Calendar
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId :: UserId
  , name :: String
  , age :: Int
  , email :: String
  , registration_date :: Day
  } deriving (Eq, Show, Generic)

instance ToJSON User

newtype UserId = UserId Int deriving (Eq, Show, ToJSON)
instance FromHttpApiData UserId where
   parseUrlPiece = fmap UserId . parseUrlPiece

instance FromHttpApiData [UserId] where
   parseUrlPiece = traverse parseUrlPiece . splitOn ","

users :: [User]
users =
  [ User (UserId 1) "Isaac Newton"    372 "isaac@newton.co.uk"  (fromGregorian 1683  3 1)
  , User (UserId 2) "Albert Einstein" 136 "ae@mc2.org"          (fromGregorian 1905 12 1)
  , User (UserId 3) "Mario Mario"     36  "mario@mariobros.com" (fromGregorian 1981  7 9)
  ]

type UserAPI1
  =    "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "userid" UserId :> Get '[JSON] User
  :<|> "users" :> Capture "userids" [UserId] :> Get '[JSON] [User]

allUsers :: Handler [User]
allUsers = return users

getUser :: UserId -> Handler User
getUser uid =
  let maybeUser = listToMaybe [u | u <- users, userId u == uid]
  in case maybeUser of
    Just user -> return user
    Nothing -> throwError err404

multiGetUsers :: [UserId] -> Handler [User]
multiGetUsers userIds =
  return [u | u <- users, elem (userId u) userIds]

server1 :: Server UserAPI1
server1 = allUsers :<|> getUser :<|> multiGetUsers

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

main :: IO ()
main = do
  let port = 8081 :: Int
  putStrLn $ "Running on port " ++ (show port) ++ "..."
  run 8081 app1
