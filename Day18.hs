-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day18 (solve) where

import Data.Function ( (&) )
import Data.List
-- import qualified Data.Char as Char
import qualified Data.List.Split as Split
-- import qualified Data.Map as Map

solve input lines = do
    let cubes = readCubes lines
    print $ cubes & map ((6 -) . countCovered cubes) & sum



countCovered cubes cube = cubes & filter ((== 1). dist cube) & length

dist (x, y, z) (x', y', z') = abs (x' - x) + abs (y' - y) + abs (z' - z)

x (n, _, _) = n :: Int
y (_, n, _) = n :: Int
z (_, _, n) = n :: Int

readCubes :: [String] -> [(Int, Int, Int)]
readCubes =
    map readLine
    where
        readLine line = do
            let [x, y, z] = line & Split.splitOn "," & map read :: [Int]
            (x, y, z)

