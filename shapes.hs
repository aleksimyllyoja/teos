module Shapes where

import Config
import ShapeUtils
import System.Random

boundingBox :: Path
boundingBox = [(0, 0), (width, 0), (width, height), (0, height), (0, 0)]

circlePoints :: Point -> Double -> Int -> Path
circlePoints (cx, cy) radius precision = circlePoints' (cx, cy) radius 0 precision

circlePoints' :: Point -> Double -> Double -> Int -> Path
circlePoints' (cx, cy) radius angle precision =
  repeatFirst [ circlePoint (cx, cy) radius (angleStep*i+angle) | i <- map fromIntegral [0..precision-1]]
  where angleStep = twopi/(fromIntegral precision)

circle100 c r = circlePoints c r 100

circlePointsByAngles :: Point -> Double -> [Double] -> Path
circlePointsByAngles (cx, cy) radius angles =
  repeatFirst [ circlePoint (cx, cy) radius a | a <- angles]

markPoints ps = [circlePoints p 2 3 | p <- ps]

bezier :: [Point] -> Double -> Path
bezier ctrlPoints precision =
  [ (beta t xs, beta t ys) |
    k <- [0..precision-1], let t = k/(precision-1) ]
  where beta = beta' 0 ((length ctrlPoints)-1)
        beta' i 0 t ps = ps !! i
        beta' i n t ps = (beta' i (n-1) t ps)*(1-t)+(beta' (i+1) (n-1) t ps)*t
        (xs, ys) = unzip ctrlPoints

stripes :: Double -> Double -> Point -> Point -> Path
stripes spacing length p1 p2 =
  foldr (++) [] $ toPaths $ reverseEveryOther [(
      p1 .+. (0, spacing*i)
    , p1 .+. (length, spacing*i)
    ) | i <- [0..n]
  ]
  where l = lineLength p1 p2
        n = l/spacing

bezier100 :: [Point] -> Path
bezier100 ctrlPoints = bezier ctrlPoints 40

bezierVariation :: Point -> Point -> [Double] -> Path
bezierVariation p1 p2 vs = bezier100 $ p1:(controlPointsFromLine p1 p2 vs)++[p2]

randomBezierVariation :: Point -> Point -> Double -> IO Path
randomBezierVariation p1 p2 magnitude = do
  g <- newStdGen
  let vs = (take 4 (map (\x -> (x-0.5)*magnitude) $ randoms g :: [Double]))
  return $ bezierVariation p1 p2 vs

bezierVariedCircle center radius magnitude n = do
  g <- newStdGen
  let variationPointCount = n
  let variations = randomVariations n variationPointCount magnitude g
  return $ bezierVariedCircle' center radius n variations

bezierVariedCircle' center radius n variations =
  foldr (++) [] [bezierVariation p1 p2 (take n rs') | ((p1, p2), rs') <- zip q variations]
  where c = circlePoints' center radius (twopi/6) n
        q = zip c (tail c)

bezierVariedPoly' center radius angles variations =
  foldr (++) [] [bezierVariation p1 p2 (take n rs') | ((p1, p2), rs') <- zip q variations]
  where c = circlePointsByAngles center radius angles
        q = zip c (tail c)
        n = length angles

bezierVariedPoly center radius magnitude n = do
  g <- newStdGen
  let variationPointCount = n
  let variations = randomVariations n variationPointCount magnitude g
  let angles = take n $ (randoms g :: [Double])
  let angleVariations =  scanl1 (+) $ map (twopi/(sum angles)*) angles
  return $ bezierVariedPoly' center radius angleVariations variations

filledBezierVariedPoly center@(x, y) radius magnitude n = do
  shape <- bezierVariedPoly center radius magnitude n
  let cutTexture = cutPathOutsideShape texture shape
  return $ cutTexture++[shape]
  where texture = stripes fillSpacing width (center .-. (tr, tr)) (center .+. (-tr, tr))
        tr = radius+magnitude

variedPolygon center radius magnitude n = do
  g <- newStdGen
  let angles = take n $ (randoms g :: [Double])
  let angleVariations =  scanl1 (+) $ map (twopi/(sum angles)*) angles
  return $ circlePointsByAngles center radius angleVariations

filledVariedPolygon center radius magnitude n = do
  shape <- variedPolygon center radius magnitude n
  let cutTexture = cutPathOutsideShape texture shape
  return $ cutTexture++[shape]
  where texture = stripes fillSpacing width (center .-. (tr, tr)) (center .+. (-tr, tr))
        tr = radius+magnitude

pythagorasTree p1 p2 angle 0 = []
pythagorasTree p1 p2 angle depth =
  [p1, p2]:
  [p2, p3]:
  [p2, p4]:
  ((pythagorasTree p2 p3 angle (depth-1))++(pythagorasTree p2 p4 angle (depth-1)))
  where p3 = circlePoint p2 radius (angle-angle)
        p4 = circlePoint p2 radius (angle+angle)
        angle = lineAngle p1 p2
        radius = (lineLength p1 p2)*0.9


box p r = circlePoints' p r (pi/4) 4

filledBox p r =
  cutTexture
  where texture = stripes fillSpacing (tr*2) (p .-. (tr, tr)) (p .+. (-tr, tr))
        tr = r+10
        shape = box p r
        cutTexture = foldr (++) [] $ cutPathOutsideShape texture shape
