type Point = (Double, Double)
type Path = [Point]

twopi = 2*pi

round4 :: Double -> Double
round4 x = fromIntegral (round $ x * 1e4) / 1e4

repeat_first :: [t] -> [t]
repeat_first a@(x:_) = a ++ [x] 

subtended_angle :: Point -> Point -> Point -> Double
subtended_angle (x1, y1) (x2, y2) (x3, y3) = 
  atan2 a b
  where
    a = (x1-x2)*(y1-y3) - (y1-y2)*(x1-x3)
    b = (x1-x2)*(x1-x3) + (y1-y2)*(y1-y3)

is_inside :: Point -> Path -> Bool
is_inside p ps = round4 (s / twopi) == 1.0
  where
    s = is_inside' p ps
    is_inside' p (a:[]) = 0
    is_inside' p (a:b:ps) = (is_inside' p (b:ps) + subtended_angle p a b)

circle :: Point -> Double -> Double -> Path
circle (cx, cy) radius precision = 
  repeat_first [ ((cos (twopi/precision*i))*radius + cx, (sin (twopi/precision*i))*radius + cy) |
  i <- [0..precision-1] ]

bezier :: [Point] -> Double -> Path
bezier ctrl_points precision = 
  [ (beta t xs, beta t ys) |
    k <- [0..precision-1], let t = k/(precision-1) ]
  where
    beta = beta' 0 ((length ctrl_points)-1)
    beta' i 0 t ps = ps !! i
    beta' i n t ps = (beta' i (n-1) t ps)*(1-t)+(beta' (i+1) (n-1) t ps)*t
    (xs, ys) = unzip ctrl_points

main :: IO ()
main = do
  putStrLn $ show $ is_inside (100, 10) (circle (100, 100) 100 100)