module Day08 (solve) where

import Data.Function
import Data.List

type Tree = ((Int, Int), Int)

solve input lines = do
    let forest = readTrees lines
    forest
        & countVisible
        & print
    forest
        & concat
        & map (scenicScore forest)
        & maximum
        & print

scenicScore forest tree@((x, y), candidateHeight) = do
    let (mxCol, mxRow) = ((length . head) forest - 1, length forest - 1)
    let up = [y-1, y-2 .. 0] & map (\y -> forest !! y !! x)
    let down = [y+1, y+2 .. mxRow] & map (\y -> forest !! y !! x)
    let left = [x-1, x-2 .. 0] & map (\x -> forest !! y !! x)
    let right = [x+1, x+2 .. mxCol] & map (\x -> forest !! y !! x)
    product $ map viewDist [up, down, left, right]
    where
        pick :: [Tree] -> [Tree]
        pick [] = []
        pick (tree@(pos, height):rest)
            | height < candidateHeight = tree : pick rest
            | otherwise = [tree]
        viewDist = length . pick

countVisible forest = do
    let flippedRows = map reverse forest
    let columns = transpose forest
    let flippedColumns = map reverse columns
    let linesOfSight = concat [forest, flippedRows, columns, flippedColumns]
    concatMap pickVisible linesOfSight & nub & length
    
pickVisible :: [Tree] -> [Tree]
pickVisible =
    pick (-1)
    where
        pick mx [] = []
        pick mx (tree@(_, h):rest) =
            if h > mx then tree : pick h rest else pick mx rest

readTrees :: [String] -> [[Tree]]
readTrees = 
    zipWith (\y line -> zipWith (\x h -> ((x, y), read [h])) [0..] line) [0..] 
