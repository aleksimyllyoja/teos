{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

import Shapes
import ShapeUtils
import DrawUtils

main = do
  b1 <- filledBezierVariedPoly (xWidth/2,    yWidth/2-20) 30 20 3
  b2 <- filledBezierVariedPoly (xWidth/2-50, yWidth/2) 40 20 4
  b3 <- filledBezierVariedPoly (xWidth/2,    yWidth/2) 50 20 5
  
  dumpJson (b1++b2++b3)

  drawPaths b1
  drawPaths b2
  drawPaths b3