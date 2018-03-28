{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

import Shapes
import ShapeUtils
import DrawUtils
import Config
import System.Random


box9 (x, y) w = [
    (x, y), (x+s, y), (x+2*s, y),
    (x, y+s), (x+s, y+s), (x+2*s, y+s),
    (x, y+2*s), (x+s, y+2*s), (x+2*s, y+2*s)
  ]
  where s = w/2

t = box9 (50, 80) 20
t2 = box9 (60, 80) 20
t3 = box9 (70, 80) 20
t4 = box9 (120, 80) 20

ls = map (\(p1, p2) -> randomBezierVariation p1 p2 140)

randomPick l = do
  g <- newStdGen
  let (r1, g1) = (randomR (0, 8) g :: (Int, StdGen))
  let (r2, g2) = (randomR (0, 8) g1 :: (Int, StdGen))
  let (r3, g3) = (randomR (0, 8) g2 :: (Int, StdGen))
  return [l !! r1]

randomBox b1 b2 b3 b4 = do
  x1 <- randomPick b1
  x2 <- randomPick b2
  x3 <- randomPick b3
  x4 <- randomPick b4

  let xs = x1++x2++x3++x4
  let l = zip xs (tail xs)
  sequence (ls l)

arrow p a l = [
    (circlePoint p l (a+a'),  p),
    (p, circlePoint p l (a-a'))
  ]
  where a' = (pi/10)

lqt :: Point -> Double -> Path
lqt p@(x, y) l = [
    (x-2, y), (x+2, y+l)
  ]

rqt :: Point -> Double -> Path
rqt p@(x, y) l = [
    (x, y), (x-2, y+l)
  ]

main = do
  path <- randomBox t t2 t3 t4

  let (p1, p2) = last $ pathToLines (last path)
  let a = toPaths $ arrow p2 (lineAngle p1 p2 - pi) 10

  let lq1 = lqt (30, 80) 5
  let lq2 = lqt (33, 80) 5

  let rq1 = rqt (140, 80) 5
  let rq2 = rqt (143, 80) 5

  let paths = [joinPaths (path++a), lq1, lq2, rq1, rq2]

  dumpJson paths
  drawPaths paths
