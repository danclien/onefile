#!/usr/bin/env stack
{- stack
  --resolver lts-9.14
  --install-ghc
  runghc
  --package base
  --package mmorph
  --package transformers
  --
  -hide-all-packages
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Morph
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

main :: IO ()
main = do
  result <- runStateT tock 0
  execWriterT (runStateT program 0)
  print result

program :: StateT Int (WriterT [Int] IO) ()
program = replicateM_ 4 $ do
  hoist lift tock
  hoist (hoist generalize) save

save :: StateT Int (Writer [Int]) ()
save = do
    n <- get
    lift $ tell [n]

tock :: StateT Int IO ()
tock = do
  hoist generalize tick
  lift $ putStrLn "Tock!"

tick :: State Int ()
tick = modify (+1)

