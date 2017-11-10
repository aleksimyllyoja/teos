module Shapes where

type Point = (Double, Double)
type Line = (Point, Point)
type Path = [Point]

twopi = 2*pi

(·) :: Point -> Point -> Double
(·) (x1, y1) (x2, y2) = x1*x2 + y1*y2

toPath :: Line -> Path
toPath (p1, p2) = [p1, p2]

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

notInside p ps = not $ isInside p ps

aSeq = [(-1)**x | x <- [1..]]

lineAngle (x1, y1) (x2, y2) = (atan2 (y1-y2) (x1-x2))+pi

lineLength (x1, y1) (x2, y2) = sqrt $ (y1-y2)**2+(x1-x2)**2

circlePoint (x, y) r a = (x+r*(cos a), y+r*(sin a))

pointByParam s m (x1, y1) (x2, y2) = (x4, y4)
  where a = lineAngle (x1, y1) (x2, y2)
        l = lineLength (x1, y1) (x2, y2)
        (x3, y3) = circlePoint (x1, y1) (s*l) a 
        (x4, y4) = circlePoint (x3, y3) m (a-pi/2)

controlPointsFromLine p1 p2 vs n = mps
  where mps = [pointByParam (l/n) v p1 p2 | (l, v) <- zip [1..n-1] vs]

circle :: Point -> Double -> Double -> Path
circle (cx, cy) radius precision = circle' (cx, cy) radius 0 precision

circle' :: Point -> Double -> Double -> Double -> Path
circle' (cx, cy) radius angle precision =
  repeatFirst [ circlePoint (cx, cy) radius (twopi/precision*i+angle) | i <- [0..precision-1] ]

bezier :: [Point] -> Double -> Path
bezier ctrlPoints precision = 
  [ (beta t xs, beta t ys) |
    k <- [0..precision-1], let t = k/(precision-1) ]
  where beta = beta' 0 ((length ctrlPoints)-1)
        beta' i 0 t ps = ps !! i
        beta' i n t ps = (beta' i (n-1) t ps)*(1-t)+(beta' (i+1) (n-1) t ps)*t
        (xs, ys) = unzip ctrlPoints

bezier100 ctrlPoints = bezier ctrlPoints 100

closestPointParam :: Line -> Point -> Double
closestPointParam ((x1, y1), (x2, y2)) (x3, y3)
  = (x3-x1, y3-y1) · (x2-x1, y2-y1) 
  / (x2-x1, y2-y1) · (x2-x1, y2-y1)

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

cutClosestEnd :: Line -> Point -> Line
cutClosestEnd (p1, p2) p3
  | closestPointParam (p1, p2) p3 > 0.5 = (p3, p2)
  | otherwise = (p1, p3)

cutOutsideShapeByIntersections :: Line -> [Point] -> Path -> Maybe Line
cutOutsideShapeByIntersections line [] _ = Just line
cutOutsideShapeByIntersections line (ip:ps) shape = cutOutsideShapeByIntersections (cutOutsideShape' line ip shape) ps shape
  where cutOutsideShape' l@(p1, p2) ip shape
          | p1 `isInside` shape && p2 `notInside` shape = (p1, ip)
          | p2 `isInside` shape && p1 `notInside` shape = (ip, p2)
          | otherwise = cutClosestEnd l ip

cutLineOutsideShape :: Line -> Path -> Maybe Line
cutLineOutsideShape l@(p1, p2) shape =
  case intersections of
    [] -> if (p1 `isInside` shape && p2 `isInside` shape) then cutOutsideShapeByIntersections l intersections shape else Nothing
    [ip] -> if (p1 `notInside` shape && p2 `notInside` shape) then Nothing else cutOutsideShapeByIntersections l intersections shape
    _ -> cutOutsideShapeByIntersections l intersections shape
  where intersections = lineShapeIntersections l shape

cutPathOutsideShape :: Path -> Path -> Path
cutPathOutsideShape (p1:[]) _ = [] 
cutPathOutsideShape (p1:p2:ps) shape =
  case l' of
    Just (p1', p2') -> p1':p2':(cutPathOutsideShape (p2:ps) shape)
    Nothing -> cutPathOutsideShape (p2:ps) shape
  where l' = cutLineOutsideShape (p1, p2) shape

cutPathsInsideShape shape = map (flip cutPathOutsideShape shape)

filledCircle :: Point -> Double -> Double -> Double -> [Path]
filledCircle (cx, cy) radius precision fc = filter (not . null) $ c:(map (flip cutPathOutsideShape c) filling)
  where c = circle (cx, cy) radius precision
        filling = map toPath [((cx-radius-10, cy-radius+fs*x), (cx+radius+10, cy-radius+fs*x)) | x <- [0..fc]]
        fs = 2*radius/fc
