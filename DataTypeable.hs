#!/usr/bin/env stack
{-
  stack
  script
  --resolver nightly-2017-12-13
  --ghc-options -Wall
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Run by executing this file in your shell
-- Tested with Stack v1.6.1

-- Working through Chris Done's post on 'Data' and 'Typeable'
-- http://chrisdone.com/posts/data-typeable

import Data.Data

data X = X deriving (Data, Typeable)

main :: IO ()
main = do
  typeableMain
  dataMain

typeableMain :: IO ()
typeableMain = do
  putStrLn "Typeable examples"
  putStrLn "------------------"

  -- `typeOf` from `Typeable`
  print $ typeOf (undefined :: Int) -- Int

  let a = typeOf 'a'
  let b = typeOf 'b'
  print $ a == b -- True

  print $ a == (typeOf ()) -- False

  -- Using 'cast' from Data.Typeable
  let char c = case cast c of
                 Just (x :: Char) -> print x
                 Nothing -> putStrLn "Unknown"

  char 'a' -- 'a'
  char (5 :: Int) -- Unknown

  putStrLn ""

data Y = Y { foo :: Int, bar :: Char } deriving (Data, Typeable)

dataMain :: IO ()
dataMain = do
  putStrLn "Data examples"
  putStrLn "--------------"

  -- 'dataTypeOf' from Data.Data
  print $ dataTypeOf $ Just 'a' -- DataType {tycon = "Maybe", datarep = AlgRep [Nothing,Just]}

  let maybeUnitValue = Nothing :: Maybe ()
  print $ dataTypeConstrs $ dataTypeOf $ maybeUnitValue -- [Nothing,Just]
  print $ indexConstr (dataTypeOf maybeUnitValue) 2 -- Just

  print $ isAlgType (dataTypeOf (Just 'a')) -- True
  print $ isAlgType (dataTypeOf 'a') -- False

  print $ toConstr (Just 'a') -- Just
  print $ toConstr (Just 'a') == toConstr (Nothing :: Maybe Char) -- False
  print $ constrType (toConstr (Just 'a')) -- DataType {tycon = "Maybe", datarep = AlgRep [Nothing,Just]}

  print $ toConstr (Y 0 'a') -- Y
  print $ constrFields (toConstr (Y 0 'a')) -- ["foo","bar"]

  print $ (fromConstr (toConstr maybeUnitValue) :: Maybe ()) -- Nothing

  

  putStrLn ""
