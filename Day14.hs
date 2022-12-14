{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Day14 (solve) where

import Data.Function ( (&) )
import Data.List

import qualified Data.Map as Map
import qualified Data.List.Split as Split
import Control.Arrow (Arrow(first))

type Cave = Map.Map (Int, Int) Char
type Pos = (Int, Int)

solve input lines = do
    let paths = readPaths lines
    let rock = concatMap expandPath paths & nub
    let floor = rock & map snd & maximum
    let cave = zip rock (repeat '#') & Map.fromList :: Cave
    let caves = unfoldr (dropSand floor) (cave, (-1, -1))
    let (lstCave, lstPos) =
            caves
            & dropWhile (\ (_, (x, y)) -> y /= floor)
            & head
    let restingSand = length lstCave - length cave

    print restingSand

dropSand :: Int -> (Cave, Pos) -> Maybe ((Cave, Pos), (Cave, Pos))
dropSand floor (cave, _) = do
    Just ((cave', lastPos), (cave', lastPos)) where
        lastPos = sandPath floor cave & last
        cave' = do
            if snd lastPos == floor then
                cave
            else
                Map.insert lastPos 'o' cave

sandPath floor cave =
    unfoldr advanceSand (500, 0) 
    where
        advanceSand (x, y) = do
            if y == floor then 
                Nothing 
            else
                [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
                    & find (\ pos -> do Map.member pos cave & not)
                    & \ case
                            (Just pos) -> Just (pos, pos)
                            Nothing -> Nothing
                
expandPath path = do
    path & pairwise & concatMap expandPair
    where
        expandPair ((x1, y1), (x2, y2))
            | x1 == x2 = map (x1,) [min y1 y1 .. max y1 y2]
            | y1 == y2 = map (, y1) [min x1 x2 .. max x1 x2]
        pairwise a = zip a (tail a)

readPaths :: [[Char]] -> [[(Int, Int)]]
readPaths lines = do
    let sectionTxts = map (Split.splitOn " -> ") lines
    sectionTxts 
        & map (map (\ xyTxt -> do
            let [xTxt, yTxt] = Split.splitOn "," xyTxt
            (read xTxt, read yTxt) )) 
