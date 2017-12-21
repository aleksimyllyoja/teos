{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

import Shapes
import ShapeUtils
import DrawUtils
import Config
import System.Random

variationMagnitude = 10
radius = xStep/3

columns = 2
rows = 2

xStep = xWidth/(rows+1)
yStep = yWidth/(columns+1)

fbvp p = filledBezierVariedPoly p radius variationMagnitude 3
avp p n = filledVariedPolygon p radius variationMagnitude n

box p r = circlePoints' p r (pi/4) 4

filledBox p r = 
  cutTexture
  where texture = stripes fillSpacing (tr*2) (p .-. (tr, tr)) (p .+. (-tr, tr))
        tr = r+10
        shape = box p r
        cutTexture = foldr (++) [] $ cutPathOutsideShape texture shape

paths = do
  g <- newStdGen
  let vs = take 4 $ (randoms g :: [Double])
  return [filledBox (x*43, y*43) (30+v*30) | ((x, y), v) <- zip ps vs]
  where ps = [(x, y) | x <- [1..2.0], y <- [1..2.0]]

main = do
  let lines = stripes 2.0 100 midPoint (midPoint .+. (100, 100))
  paths' <- paths
  let ps = (foldr (++) [] paths')
  dumpJson paths'
  drawPaths paths'
  -- drawPaths paths