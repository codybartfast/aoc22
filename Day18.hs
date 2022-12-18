{-# LANGUAGE TupleSections #-}
module Day18 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.List.Split as Split
import qualified Data.Set as Set

solve input lines = do
    let cubes = readCubes lines
    let uncovered cubes = cubes & map ((6 -) . countCovered cubes) & sum
    print $ uncovered cubes

    let maxs = maximums cubes
    let water = findWater cubes maxs
    let notWater = complement water maxs & Set.toList
    print $ uncovered notWater

findWater cubeList (maxX, maxY, maxZ)= do
    let expansions = unfoldr expand (Set.singleton (0, 0, 0))
    let pairs = pairwise expansions
    let (Just (water, _)) = find ( \ (a, b) -> a == b) pairs
    water
    where
        cubes = Set.fromList cubeList

        expand water = do
            let newWater = water & concatMap (expandPos water) & nub
            let water' = foldl (flip Set.insert) water newWater
            Just (water', water')
        
        expandPos water pos = do
            neighbours pos
            & filter (\ candidate@(x, y, z) ->
                0 <= x && x <= maxX
                && 0 <= y && y <= maxY
                && 0 <= z && z <= maxZ
                && not (Set.member candidate water)
                && not (Set.member candidate cubes))

complement water (maxX, maxY, maxZ) =
    [0 .. maxZ] & concatMap (\ z ->
        [0 .. maxY] & concatMap (\ y -> 
            [0 .. maxX] & map (, y, z) ))
    & Set.fromList
    & (`Set.difference` water)

neighbours (x, y, z) =
    map (\ (dx, dy, dz) -> (x + dx, y + dy, z + dz))
        [ (1, 0, 0), (0, 1, 0), (0, 0, 1) , (-1, 0, 0), (0, -1, 0), (0, 0, -1)]

countCovered cubes cube = cubes & filter ((== 1). dist cube) & length

dist (x, y, z) (x', y', z') = abs (x' - x) + abs (y' - y) + abs (z' - z)

maximums coords = do
    let maxX = coords & map x & maximum
    let maxY = coords & map y & maximum
    let maxZ = coords & map z & maximum
    (maxX, maxY, maxZ)
    where
        x (n, _, _) = n :: Int
        y (_, n, _) = n :: Int
        z (_, _, n) = n :: Int

pairwise lst = zip lst (tail lst)

readCubes =
    map readLine
    where
        readLine line = do
            let [x, y, z] = line & Split.splitOn "," & map read :: [Int]
            (x, y, z)
