module Day08 (solve) where

import Data.Function
import Data.List

solve input lines = do
    let trees = labelTrees lines
    let flippedRows = map reverse trees
    let columns = transpose trees
    let flippedColumns = map reverse columns
    let linesOfSight = concat [trees, flippedRows, columns, flippedColumns]
    -- let visible = map pickVisible flippedRows & concat & nub
    let visible = map pickVisible linesOfSight & concat & nub

    print (length visible)
    print visible

pickVisible :: [((Int, Int), Int)] -> [((Int, Int), Int)]
pickVisible trees = do
    (head trees) : (
        nwise 2 trees
        & takeWhile (\[(_, h1), (_, h2)] -> h1 < h2)
        & map (head. tail))

labelTrees :: [String] -> [[((Int, Int), Int)]]
labelTrees lines = do
    let labelLine = zip [0..]
    map (zip [0..]) lines 
        & zipWith (\y pairs -> map (\(x, t) -> ((x, y), read [t])) pairs ) [0..]

nwise n list =
    tails list
    & map (take n)
    & dropWhileEnd ((< n). length)
