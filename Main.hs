{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

import Shapes
import DrawUtils

p1 = (xWidth/2-100,  yWidth/2)
p2 = (xWidth/2-20,   yWidth/2)

generateTest = do
  let trunk = pythagorasTree p1 p2 6
  foliage <- filledBezierVariedCircle (xWidth/2+35, yWidth/2) 70 10 8
  return $ trunk++foliage

main = do
  paths <- generateTest
  -- drawPaths paths
  dumpJson paths