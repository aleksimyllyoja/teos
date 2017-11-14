{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

module DrawUtils where

import ShapeUtils
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
import Data.Maybe
import Control.Monad (liftM)

apiPath = "http://localhost:4242/v1"

penPath = apiPath ++ "/pen/"
bufferPath = apiPath ++ "/buffer/"

xWidth = 300.0
yWidth = 218.0

xHalf = xWidth/2
yHalf = yWidth/2

midPoint = (xHalf, yHalf)

xWidthSteps = 12000.0
yWidthSteps = 8720.0

penUpValue = 0.0
penDownValue = 1

penDelay = 300*10^4

data Buffer = Buffer {
    running :: Bool
  } deriving (Generic, Show)

instance FromJSON Buffer

pathToJson :: Path -> String
pathToJson = (replace ")" "]") . (replace "(" "[") . show

pathsToJson :: [Path] -> String
pathsToJson = (join ",") . map pathToJson

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
  penUp
  putStrLn "Enter pen"
  getLine
  
  drawPaths' paths
  movePen (0, 0)

dumpJson :: [Path] -> IO ()
dumpJson paths = do 
  writeFile "preview.js" ("DATA = ["++str++"];")
  where str = pathsToJson $ paths

tupleToList = foldr (\(f,s) a -> f:s:a) []

addPaintFetchesToPath paths = tupleToList (zip fs paths)
  where fs = repeat ((circle100 (20, 20) 19)++(reverse $ circle100 (23, 23) 21))
