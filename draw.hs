{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Shapes
import Data.Aeson (encode, decode, object, (.=), Object, Value, ToJSON, FromJSON)
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString.Lazy.Char8 as L8
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
movePenRequest p = object ["x" .= (x :: Double) , "y" .= (y :: Double)]
  where (x, y) = mmToPercentage p

setPenStateRequest :: Double -> Value
setPenStateRequest v = object ["state" .= (v :: Double)]

toDevice method url manager payload = do
  initialRequest <- parseRequest url
  let request = initialRequest
          { method = method
          , requestBody = RequestBodyLBS $ encode payload
          , requestHeaders =
              [ ("Content-Type", "application/json; charset=utf-8")
              ]
          }
  httpLbs request manager

sendToDevice = toDevice "PUT"

getBuffer manager = do toDevice "GET" bufferPath manager emptyObject

movePen manager point = do sendToDevice penPath manager (movePenRequest point)
setPenState manager v = do sendToDevice penPath manager (setPenStateRequest v)
penUp manager = do setPenState manager penUpValue
penDown manager = do setPenState manager penDownValue

drawPath manager [] = do return ()
drawPath manager (p:ps) = do
  movePen manager p
  drawPath manager ps

drawPaths manager [] = do return ()
drawPaths manager (path:paths) = do
  movePen manager (head path)
  penDown manager
  drawPath manager path
  penUp manager
  drawPaths manager paths