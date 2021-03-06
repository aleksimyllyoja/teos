{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

module DrawUtils where
import Config

import Data.Aeson (encode, decode, object, (.=), Object, Value, ToJSON, FromJSON)
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Internal as BS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import Control.Concurrent
import GHC.Generics
import Data.String.Utils
import Data.Maybe
import Control.Monad (liftM)

data Buffer = Buffer {
    running :: Bool
  } deriving (Generic, Show)

instance FromJSON Buffer

type Point = (Double, Double)
type Line = (Point, Point)
type Path = [Point]

pathToJson :: Path -> String
pathToJson = (replace ")" "]") . (replace "(" "[") . show

pathsToJson :: [Path] -> String
pathsToJson = (join ",") . map pathToJson

mmToPercentage :: Point -> Point
mmToPercentage (x, y) = ((max (min x width) 0)/width*100, (max (min y height) 0)/height*100)

movePenRequest :: Point -> Value
movePenRequest p = object ["x" .= (x :: Double), "y" .= (y :: Double)]
  where (x, y) = mmToPercentage p

setPenStateRequest :: Double -> Value
setPenStateRequest v = object ["state" .= (v :: Double)]

toDevice :: (?manager :: Manager) => BS.ByteString -> String -> Value -> IO (Response L8.ByteString)
toDevice method url payload = do
  initialRequest <- parseRequest url
  let request = initialRequest
          { method = method
          , requestBody = RequestBodyLBS $ encode payload
          , requestHeaders =
              [ ("Content-Type", "application/json; charset=utf-8")
              ]
          }
  httpLbs request ?manager

getFromDevice :: (?manager :: Manager) => String -> Value -> IO (Response L8.ByteString)
getFromDevice = toDevice "GET"

putToDevice :: (?manager :: Manager) => String -> Value -> IO (Response L8.ByteString)
putToDevice = toDevice "PUT"

movePen :: (?manager :: Manager) => Point -> IO (Response L8.ByteString)
movePen point = do putToDevice penPath (movePenRequest point)

setPenState :: (?manager :: Manager) => Double -> IO (Response L8.ByteString)
setPenState v = do putToDevice penPath (setPenStateRequest v)

isRunning :: (?manager :: Manager) => IO Bool
isRunning = do
  a <- getFromDevice bufferPath emptyObject
  return (fromMaybe False (liftM running (decode (responseBody a) :: Maybe Buffer)))

wait :: (?manager :: Manager) => IO ()
wait = do
  r <- isRunning
  case r of
    False -> return ()
    True -> wait

penUp :: (?manager :: Manager) => IO (Response L8.ByteString)
penUp = do setPenState penUpValue

penDown :: (?manager :: Manager) => IO (Response L8.ByteString)
penDown = do setPenState penDownValue

drawPath :: (?manager :: Manager) => Path -> IO (Response L8.ByteString)
drawPath [] = do penUp
drawPath (p:ps) = do
  movePen p
  drawPath ps

drawPaths' :: (?manager :: Manager) => [Path] -> IO (Response L8.ByteString)
drawPaths' [] = do penUp
drawPaths' ([]:paths) = drawPaths' paths
drawPaths' ((p1:path):paths) = do
  movePen p1
  wait
  penDown
  threadDelay penDelay
  drawPath path
  penUp
  drawPaths' paths

drawPaths :: [Path] -> IO (Response L8.ByteString)
drawPaths paths = do
  manager <- newManager tlsManagerSettings
  let ?manager = manager

  movePen (0, 0)
  drawPaths' paths
  movePen (0, 0)

dumpJson :: [Path] -> IO ()
dumpJson paths = do
  writeFile "preview.js" ("DATA = ["++str++"];")
  where str = pathsToJson $ paths
