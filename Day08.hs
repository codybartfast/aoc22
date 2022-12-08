module Day08 (solve) where

import Data.Function
import Data.List
import Control.Monad (when)

solve input lines = do
    let trees = labelTrees lines
    let flippedRows = map reverse trees
    let columns = transpose trees
    let flippedColumns = map reverse columns
    let linesOfSight = concat [trees, flippedRows, columns, flippedColumns]
    let visible = map pickVisible linesOfSight & concat & nub
    print (length visible)

pickVisible :: [((Int, Int), Int)] -> [((Int, Int), Int)]
pickVisible =
    pick [] (-1)
    where
        pick rslts mx [] = rslts
        pick rslts mx (tree@(_, h):rest) =
            if h > mx then pick (tree:rslts) h rest else pick rslts mx rest

labelTrees :: [String] -> [[((Int, Int), Int)]]
labelTrees lines = do
    let labelLine = zip [0..]
    map (zip [0..]) lines 
        & zipWith (\y pairs -> map (\(x, t) -> ((x, y), read [t])) pairs ) [0..]
