#!/usr/bin/env stack
{- stack
  --resolver lts-10.3
  --install-ghc
  runghc
  --package base
  --package lens
  --package mtl
  --package pipes
  --package pipes-bytestring
  --package pipes-parse
  --package random-strings
  --package utf8-string
  --
  -hide-all-packages
-}


{-

Example of using `pipes-parse` to write a stream into multiple files

-}

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Control.Lens ((^.))
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Function (on)
import Data.Monoid ((<>))
import Pipes
import qualified Pipes.ByteString as P (toHandle)
import qualified Pipes.Prelude as P (drain, map, take)
import qualified Pipes.Parse as P
import System.IO (IOMode(WriteMode), withFile)
import Test.RandomStrings (onlyAlpha, randomASCII, randomString)
import qualified Control.Monad.State.Strict as State

data Data = Data
  { timestamp :: Int -- seconds since Unix epoch
  , value :: ByteString
  , bucket :: Int -- minutes since Unix epoch
  } deriving (Eq, Show)

main :: IO ()
main = do
  void $ State.runStateT go (splitStream inputProducer)
  putStrLn "Done"

-- | Write one bucket at a time
go :: State.StateT (Producer Data IO (Producer Data IO x)) IO ()
go = do
  isEmpty <- P.isEndOfInput
  if isEmpty
    then return ()
    else do
      headStream <- State.get
      tailStream <- lift $ writeToFile headStream
      State.put (splitStream tailStream)
      go

splitStream :: Monad m => Producer Data m x -> Producer Data m (Producer Data m x)
splitStream stream = stream ^. P.groupBy ((==) `on` bucket)

-- | Generates test data
inputProducer :: Producer Data IO ()
inputProducer = each [0..] >-> P.take 300 >-> generateData
  where
    generateData = for cat $ \i -> do
      -- Using random strings for the @value@
      randomValue <- lift $ randomString (onlyAlpha randomASCII) 30
      let result = Data i (fromString $ randomValue ++ "\n") (i `div` 60)
      lift $ print result
      yield result

-- | Write the stream to a file
writeToFile :: Producer Data IO a -> IO a
writeToFile stream = do
  maybeFilename <- readFilename stream
  case maybeFilename of
    -- If the stream is empty, run still run it for the return result
    Nothing -> runEffect $ stream >-> P.drain
    -- If the stream had items, create the file and write the contents
    Just filename ->
      withFile ("temp/" ++ filename) WriteMode $ \hWrite ->
        runEffect $ stream >-> P.map toLine >-> P.toHandle hWrite

-- | Formats @Data@ in the file
toLine :: Data -> ByteString
toLine input = (fromString . show . timestamp $ input) <> ": " <> (value input)

-- | Peek the stream to generate our bucket filename
readFilename :: Producer Data IO a -> IO (Maybe String)
readFilename stream = do
  eitherHead <- next stream
  return $ case eitherHead of
    Left _ -> Nothing
    Right (first, _) -> Just $ (show . bucket $ first) <> ".txt"

