{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

import Shapes
import ShapeUtils
import DrawUtils
import Config

variationMagnitude = 10
radius = xStep/3

columns = 4
rows = 4

xStep = xWidth/(rows+1)
yStep = yWidth/(columns+1)

fbvp p = filledBezierVariedPoly p radius variationMagnitude 3
avp p n = filledVariedPolygon p radius variationMagnitude n

generate = do
  sequence [avp (x*xStep, y*yStep) (floor (y+2*x)) |
            x <- [1..rows], y <- [1..columns]]

main = do
  paths <- generate
  dumpJson (foldr (++) [] paths)
  -- mapM drawPaths paths