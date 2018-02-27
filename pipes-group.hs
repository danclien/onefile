#!/usr/bin/env stack
{- stack
  --resolver lts-10.3
  --install-ghc
  runghc
  --package base
  --package free
  --package lens
  --package pipes
  --package pipes-bytestring
  --package pipes-group
  --package random-strings
  --package utf8-string
  --
  -hide-all-packages
-}


{-

Example of using `pipes-group` to write a stream into multiple files

-}

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens ((^.))
import Control.Monad.Trans.Free (FreeF(..), FreeT(..), runFreeT)
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Function (on)
import Data.Monoid ((<>))
import Pipes
import qualified Pipes.ByteString as P (toHandle)
import qualified Pipes.Prelude as P (drain, map, take)
import qualified Pipes.Group as P (groupsBy)
import System.IO (IOMode(WriteMode), withFile)
import Test.RandomStrings (onlyAlpha, randomASCII, randomString)

data Data = Data
  { timestamp :: Int -- seconds since Unix epoch
  , value :: ByteString
  , bucket :: Int -- minutes since Unix epoch
  } deriving (Eq, Show)

-- | Iterates through the @FreeT@ structure using manual recursion and writes each
-- bucket to a separate file
main :: IO ()
main = go groupedByBucket
  where
    go f = do
      x <- runFreeT f
      case x of
        Pure () -> return ()
        Free p -> do
          f' <- writeToFile p
          go f'

-- | Group the producers by @bucket@
groupedByBucket :: FreeT (Producer Data IO) IO ()
groupedByBucket = inputProducer ^. P.groupsBy ((==) `on` bucket)

-- | Generates test data
inputProducer :: Producer Data IO ()
inputProducer = each [0..] >-> P.take 300 >-> generateData
  where
    generateData = for cat $ \i -> do
      randomValue <- lift $ randomString (onlyAlpha randomASCII) 30
      let result = Data i (fromString $ randomValue ++ "\n") (i `div` 60)
      lift $ putStrLn $ "Generated: " ++ (show result)
      yield $ result

-- | Write the stream to a file
writeToFile :: Producer Data IO a -> IO a
writeToFile stream = do
  maybeFilename <- readFilename stream
  case maybeFilename of
    Nothing -> runEffect $ stream >-> P.drain
    Just filename -> do
      withFile ("temp/" ++ filename) WriteMode $ \hWrite ->
        runEffect $ stream >-> P.map toLine >-> P.toHandle hWrite

-- | Formats @Data@ in the file
toLine :: Data -> ByteString
toLine input = (fromString . show . timestamp $ input) <> ": " <> (value input)

-- | Peek the stream to generate the filename
readFilename :: Producer Data IO a -> IO (Maybe String)
readFilename stream = do
  eitherHead <- next stream
  return $ case eitherHead of
    Left _ -> Nothing
    Right (first, _) -> Just $ (show . bucket $ first) <> ".txt"

