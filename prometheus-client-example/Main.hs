 -- This example demonstrates how to use the prometheus-haskell libraries to
-- instrument a simple web app that allows users to vote for their favorite
-- color.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)
import System.Random

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Wai as Wai
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P

{-# NOINLINE pageVisits #-}
pageVisits :: P.Counter
pageVisits = P.unsafeRegister
           $ P.counter
           -- Each metric provided by the base library takes an Info value that
           -- gives the name of the metric and a help string that describes the
           -- value that the metric represents.
           $ P.Info "page_visits" "The number of visits to the index page."

{-# NOINLINE votes #-}
votes :: P.Vector P.Label1 P.Counter
votes = P.unsafeRegister
      -- Declare a vector of counters with a single dimension: "vote".
      $ P.vector "vote"
      $ P.counter
      $ P.Info "votes" "The number of votes for each color."


{-# NOINLINE responseTime #-}
responseTime :: P.Vector P.Label1 P.Histogram
responseTime = P.unsafeRegister
      -- Declare a vector of counters with a single dimension: "vote".
      $ P.vector "vote"
      $ P.histogram
      (P.Info "response_time" "The time for the server to response for each color.")
      P.defaultBuckets

data VectorWithLabel l m = VectorWithLabel (P.Vector l m) l

instance (P.Label l, P.Observer m) => P.Observer (VectorWithLabel l m) where
    -- | Add a new observation to a histogram metric.
    observe (VectorWithLabel vector label) value = P.withLabel vector label f
      where f metric = P.observe metric value


main :: IO ()
main = do
    let port = 2000
    putStrLn $ "Listening at http://localhost:" ++ show port ++ "/"
    -- Register the GHC runtime metrics. For these to work, the app must be run
    -- with the `+RTS -T` command line options.
    _ <- P.register P.ghcMetrics
    -- Instrument the app with the prometheus middlware using the default
    -- `PrometheusSettings`. This will cause the app to dump the metrics when
    -- the /metrics endpoint is accessed.

    let appWithPrometheus = P.prometheus P.def app
    run port appWithPrometheus

app :: Wai.Application
app request respond = do
    response <- case Wai.pathInfo request of
        []        -> doIndex
        ["red"]   -> doRed
        ["green"] -> doGreen
        ["blue"]  -> doBlue
        _         -> return $ mkResponse "404"
    respond response

mkResponse :: LBS.ByteString -> Wai.Response
mkResponse =  Wai.responseLBS status200 [(hContentType, "text/html")]

doIndex :: IO Wai.Response
doIndex = do
    P.incCounter pageVisits
    return $ mkResponse $ LBS.concat [
            "<a href='/metrics'>Metrics</a>"
        ,   "<br><br><br>"
        ,   "What's your favorite color?"
        ,   "<br>"
        ,   "<a href='/red'>Red!</a>"
        ,   "<br>"
        ,   "<a href='/blue'>Blue!</a>"
        ,   "<br>"
        ,   "<a href='/green'>Green!</a>"
        ]

doRed :: IO Wai.Response
doRed = P.observeDuration (VectorWithLabel responseTime "red") $ do
  P.withLabel votes "red" P.incCounter
  waitRandom
  return $ mkResponse "Red is alright I guess. <a href='/'>back</a>"

doBlue :: IO Wai.Response
doBlue = P.observeDuration (VectorWithLabel responseTime "blue") $ do
  P.withLabel votes "blue" P.incCounter
  waitRandom
  return $ mkResponse "Blue is whatever. <a href='/'>back</a>"

doGreen :: IO Wai.Response
doGreen = P.observeDuration (VectorWithLabel responseTime "green") $ do
  P.withLabel votes "green" P.incCounter
  waitRandom
  return $ mkResponse "Green's ok. <a href='/'>back</a>" 

waitRandom :: IO ()
waitRandom = do
  us <- getStdRandom (randomR(0,500000))
  threadDelay us
