module Config where

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
penDownValue = 1.0

penDelay :: Int
penDelay = 10*10^4

fillSpacing = 0.7