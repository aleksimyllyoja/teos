module Shapes where

import ShapeUtils
import System.Random

circlePoints :: Point -> Double -> Int -> Path
circlePoints (cx, cy) radius precision = circlePoints' (cx, cy) radius 0 precision

circlePoints' :: Point -> Double -> Double -> Int -> Path
circlePoints' (cx, cy) radius angle precision =
  repeatFirst [ circlePoint (cx, cy) radius (angleStep*i+angle) | i <- map fromIntegral [0..precision-1]]
  where angleStep = twopi/(fromIntegral precision)

circle100 c r = circlePoints c r 100

bezier :: [Point] -> Double -> Path
bezier ctrlPoints precision = 
  [ (beta t xs, beta t ys) |
    k <- [0..precision-1], let t = k/(precision-1) ]
  where beta = beta' 0 ((length ctrlPoints)-1)
        beta' i 0 t ps = ps !! i
        beta' i n t ps = (beta' i (n-1) t ps)*(1-t)+(beta' (i+1) (n-1) t ps)*t
        (xs, ys) = unzip ctrlPoints

bezier100 ctrlPoints = bezier ctrlPoints 100

bezierVariation p1 p2 vs = bezier100 $ p1:(controlPointsFromLine p1 p2 vs)++[p2]

bezierVariedCircle center radius magnitude n = do
  g <- newStdGen
  let variationPointCount = n
  let variations = randomVariations n variationPointCount magnitude g
  return $ bezierVariedCircle' center radius n variations

bezierVariedCircle' center radius n variations =
  foldr (++) [] [bezierVariation p1 p2 (take n rs') | ((p1, p2), rs') <- zip q variations]
  where c = circlePoints' center radius (twopi/6) n
        q = zip c (tail c)

filledBezierVariedCircle center@(x, y) radius magnitude n = do
  g <- newStdGen
  let variationPointCount = n
  let variations = randomVariations n variationPointCount magnitude g
  let shape = bezierVariedCircle' center radius n variations

  let texture = [[(x-radius, y-radius+i*4), (x+radius, y-radius+i*4)] | i <- [1..120]]
  let cutTexture = cutPathsOutsideShape shape texture

  return (cutTexture++[shape])

pythagorasTree p1 p2 0 = []
pythagorasTree p1 p2 depth = 
  [p1, p2]:
  [p2, p3]:
  [p2, p4]:
  ((pythagorasTree p2 p3 (depth-1))++(pythagorasTree p2 p4 (depth-1)))
  where p3 = circlePoint p2 radius (angle-pi/7.5)
        p4 = circlePoint p2 radius (angle+pi/7.5)
        angle = lineAngle p1 p2
        radius = (lineLength p1 p2)*0.55
