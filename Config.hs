module Config where

apiPath = "http://localhost:4242/v1"

penPath = apiPath ++ "/pen/"
bufferPath = apiPath ++ "/buffer/"

width = 300.0
height = 218.0

xHalf = width/2
yHalf = height/2

midPoint = (width/2, height/2)

xWidthSteps = 12000.0
yWidthSteps = 8720.0

penUpValue = 0.0
penDownValue = 1.0

penDelay :: Int
penDelay = 100*10^4

fillSpacing = 2.0
