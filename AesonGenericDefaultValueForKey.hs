#!/usr/bin/env stack
{-
  stack
  script
  --resolver nightly-2017-12-13
  --ghc-options -Wall
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

I ran into a situation at work where we have JSON stored in a MySQL table,
and we needed to add a required field with a default value. Here's a quick
experiment to default the value of the new field during parsing when using
Generic derivation.

-}

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Hashable
import Data.HashMap.Lazy
import GHC.Generics

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Eq, Show, Generic)

-- Explicitly use @genericParseJSON defaultOptions@ so we can modify
-- the 'Value' before parsing
instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions . setDefaultAge
    where
      -- 'setDefaultAge' modifies the underlaying 'HashMap' to set the default value
      setDefaultAge (Object hashmap) = (Object $ insertDefault "age" (Number 42) hashmap) 
      setDefaultAge value = value

-- Insert the value if the key doesn't already exist
insertDefault :: (Hashable k, Eq k) => k -> v -> HashMap k v -> HashMap k v
insertDefault = insertWith f
  where f _ old = old

main :: IO ()
main = do

  let fullRawJson = "{\"name\": \"Jan\", \"age\": 20}"
      fullPerson :: Maybe Person = decode fullRawJson
      partialRawJson = "{\"name\": \"Sue\"}"
      partialPerson :: Maybe Person = decode partialRawJson

  printInputOutput fullRawJson fullPerson
  -- Input: {"name": "Jan", "age": 20}
  -- Output: Just (Person {name = "Jan", age = 20})

  printInputOutput partialRawJson partialPerson
  -- Input: {"name": "Sue"}
  -- Output: Just (Person {name = "Sue", age = 42})

printInputOutput :: Show a => ByteString -> a -> IO ()
printInputOutput input output = do
  putStrLn $ "Input: " ++ (read . show $ input)
  putStr "Output: "
  print output
  putStrLn ""
