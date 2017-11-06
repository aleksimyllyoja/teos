{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

module Draw where

import Shapes
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

apiPath = "http://localhost:4242/v1"

penPath = apiPath ++ "/pen/"
bufferPath = apiPath ++ "/buffer/"

xWidth = 300.0
yWidth = 218.0

xWidthSteps = 12000.0
yWidthSteps = 8720.0

penUpValue = 0.2
penDownValue = 1

pathToJson :: Path -> String
pathToJson = (replace ")" "]") . (replace "(" "[") . show

pathsToJson :: [Path] -> String
pathsToJson = (join ",") . map pathToJson

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

scale :: Double -> Path -> Path
scale r ps = map (mapTuple (*r)) ps

mmToPercentage :: Point -> Point
mmToPercentage (x, y) = ((max (min x xWidth) 0)/xWidth*100, (max (min y yWidth) 0)/yWidth*100)

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

sendToDevice :: (?manager :: Manager) => String -> Value -> IO (Response L8.ByteString)
sendToDevice = toDevice "PUT"

movePen :: (?manager :: Manager) => Point -> IO (Response L8.ByteString)
movePen point = do sendToDevice penPath (movePenRequest point)

setPenState :: (?manager :: Manager) => Double -> IO (Response L8.ByteString)
setPenState v = do sendToDevice penPath (setPenStateRequest v)

penUp :: (?manager :: Manager) => IO (Response L8.ByteString)
penUp = do setPenState penUpValue

penDown :: (?manager :: Manager) => IO (Response L8.ByteString)
penDown = do setPenState penDownValue

drawPath :: (?manager :: Manager) => Path -> IO (Response L8.ByteString)
drawPath [] = do penUp
drawPath (p:ps) = do
  movePen p
  drawPath ps

drawPaths :: (?manager :: Manager) => [Path] -> IO (Response L8.ByteString)
drawPaths [] = do penUp
drawPaths ((p1:path):paths) = do
  movePen p1
  penDown
  drawPath path
  penUp
  drawPaths paths