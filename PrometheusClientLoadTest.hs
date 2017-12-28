#!/usr/bin/env stack
{- stack
  script
  --resolver nightly-2017-12-13
  --ghc-options -Wall
  --ghc-options -threaded
  --ghc-options -with-rtsopts=-N
-}

{-

Check to see if prometheus-client correctly counts
all 'incCounter' calls under high load

-}

import Control.Concurrent
import Control.Concurrent.Async
import Prometheus

main :: IO ()
main = do
  -- Check we're running on multiple threads
  numThreads <- getNumCapabilities
  putStrLn $ "Running on " ++ (show numThreads) ++ " threads"

  -- Create the 'Counter'
  testCounter <- counter (Info "test_counter" "This should equal 1,000,000")

  -- Increment 1,000,000 times using all threads
  putStrLn "Incrementing... Please wait."
  replicateConcurrently_ 1000000 (incCounter testCounter)

  -- Print the result
  result <- getCounter testCounter
  putStrLn "Expected: 1000000.0"
  putStrLn $ "Actual: " ++ (show result)
