module ShapeUtils where

import System.Random
import Data.List

type Point = (Double, Double)
type Line = (Point, Point)
type Path = [Point]

twopi = 2*pi

(·) :: Point -> Point -> Double
(x1, y1) · (x2, y2) = x1*x2 + y1*y2

(.-.) :: Point -> Point -> Point
(x1, y1) .-. (x2, y2) = (x1-x2, y1-y2)

(.+.) :: Point -> Point -> Point
(x1, y1) .+. (x2, y2) = (x1+x2, y1+y2)

toPath :: Line -> Path
toPath (p1, p2) = [p1, p2]

toPaths = map toPath

pathToLines :: Path -> [Line]
pathToLines path = [l | l <- zip path (tail path)]

reverseLine (p1, p2) = (p2, p1)

reverseEveryOther (a:b:t) = a:(reverseLine b):(reverseEveryOther t)
reverseEveryOther _ = []

joinPaths paths = foldr (++) [] paths

round4 :: Double -> Double
round4 x = fromIntegral (round $ x * 1e4) / 1e4

repeatFirst :: [t] -> [t]
repeatFirst a@(x:_) = a ++ [x]

subtendedAngle :: Point -> Point -> Point -> Double
subtendedAngle (x1, y1) (x2, y2) (x3, y3) =
  atan2 a b
  where a = (x1-x2)*(y1-y3) - (y1-y2)*(x1-x3)
        b = (x1-x2)*(x1-x3) + (y1-y2)*(y1-y3)

isInside :: Point -> Path -> Bool
isInside p ps = round4 (s / twopi) == 1.0
  where s = isInside' p ps
        isInside' p (a:[]) = 0
        isInside' p (a:b:ps) = (isInside' p (b:ps) + subtendedAngle p a b)

p `notInside` ps = not $ isInside p ps

lineAngle :: Point -> Point -> Double
lineAngle (x1, y1) (x2, y2) = (atan2 (y1-y2) (x1-x2))+pi

lineLength :: Point -> Point -> Double
lineLength (x1, y1) (x2, y2) = sqrt $ (y1-y2)**2+(x1-x2)**2

circlePoint :: Point -> Double -> Double -> Point
circlePoint (x, y) r a = (x+r*(cos a), y+r*(sin a))

pointByParam :: Point -> Point -> Double -> Point
pointByParam (x1, y1) (x2, y2) s = circlePoint (x1, y1) (s*l) a
  where a = lineAngle (x1, y1) (x2, y2)
        l = lineLength (x1, y1) (x2, y2)

controlPointByParam :: Double -> Double -> Point -> Point -> Point
controlPointByParam s m (x1, y1) (x2, y2) = (x4, y4)
  where a = lineAngle (x1, y1) (x2, y2)
        l = lineLength (x1, y1) (x2, y2)
        (x3, y3) = circlePoint (x1, y1) (s*l) a
        (x4, y4) = circlePoint (x3, y3) m (a-pi/2)

controlPointsFromLine :: Point -> Point -> [Double] -> [Point]
controlPointsFromLine p1 p2 vs = mps
  where mps = [controlPointByParam (l/n) v p1 p2 | (l, v) <- zip [1..n-1] vs]
        n = fromIntegral $ length vs

shorten :: Double -> Point -> Point -> Line
shorten s p1 p2 = (p1, controlPointByParam s 0 p1 p2)

closestPointParam :: Line -> Point -> Double
closestPointParam (p1, p2) p3
  = (p3 .-. p1) · (p2 .-. p1)
  / (p2 .-. p1) · (p2 .-. p1)

lineIntersection :: Line -> Line -> Point
lineIntersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = (x, y)
  where
    x = ((x2*y1-x1*y2)*(x4-x3)-(x4*y3-x3*y4)*(x2-x1))/((x2-x1)*(y4-y3)-(x4-x3)*(y2-y1))
    y = ((x2*y1-x1*y2)*(y4-y3)-(x4*y3-x3*y4)*(y2-y1))/((x2-x1)*(y4-y3)-(x4-x3)*(y2-y1))

segmentIntersection :: Line -> Line -> Maybe Point
segmentIntersection (p1, p2) (p3, p4)
  | t12 >= 0 && t12 <= 1, t23 >= 0 && t23 <= 1 = Just p0
  | otherwise = Nothing
  where p0 = lineIntersection (p1, p2) (p3, p4)
        t12 = closestPointParam (p1, p2) p0
        t23 = closestPointParam (p3, p4) p0

lineShapeIntersections :: Line -> Path -> [Point]
lineShapeIntersections _ (p:[]) = []
lineShapeIntersections (p1, p2) (p3:p4:ps)
  | Just ip <- segmentIntersection (p1, p2) (p3, p4) = ip:(lineShapeIntersections (p1, p2) (p4:ps))
  | otherwise = (lineShapeIntersections (p1, p2) (p4:ps))

cutOutsideShapeByControlPoints :: [Point] -> Path -> [Line]
cutOutsideShapeByControlPoints [] _ = []
cutOutsideShapeByControlPoints [p] _ = []
cutOutsideShapeByControlPoints (p1:p2:ps) shape =
  case newLine of
    Just l -> l:(cutOutsideShapeByControlPoints (p2:ps) shape)
    Nothing -> cutOutsideShapeByControlPoints (p2:ps) shape
  where newLine
          | tp `isInside` shape = Just (p1, p2)
          | otherwise = Nothing
        tp = pointByParam p1 p2 0.5

cutLineOutsideShape :: Line -> Path -> [Line]
cutLineOutsideShape line@(p1, p2) shape = cutOutsideShapeByControlPoints controlPoints shape
  where intersections = lineShapeIntersections line shape
        controlPoints = p1:(sortBy predicate intersections)++[p2]
        predicate x y = lineLength p1 x `compare` lineLength p1 y

cutLinesOutsideShape :: [Line] -> Path -> [Line]
cutLinesOutsideShape lines shape = foldr (++) [] $ map (\l -> cutLineOutsideShape l shape) lines

cutPathOutsideShape :: Path -> Path -> [Path]
cutPathOutsideShape path shape = map toPath $ cutLinesOutsideShape lines shape
  where lines = pathToLines path

randomVariations 0 _ _ _ = []
randomVariations chunkCount elementCount magnitude g =
    vs:(randomVariations (chunkCount-1) elementCount magnitude g2)
      where (g1, g2) = split g
            vs = (take elementCount (map (\x -> (x-0.5)*magnitude) $ randoms g1 :: [Double]))
