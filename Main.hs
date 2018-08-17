{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

import DrawUtils
import Config
import System.Environment

import Data.Aeson
import qualified Data.ByteString.Lazy as B

toTuple [a,b] = (a,b)

main = do
  args <- getArgs
  let filename = head args

  cont <- B.readFile filename

  let paths = (decode cont) :: Maybe [[[Double]]]

  case paths of
    Just xs -> drawPaths (map (map toTuple) xs)
