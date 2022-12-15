{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Day14 (solve) where

import Data.Function ( (&) )
import Data.List

import qualified Data.Map as Map
import qualified Data.List.Split as Split
import Control.Monad (forM_)

type Cave = Map.Map (Int, Int) Char
type Pos = (Int, Int)

solve input lines = do
    let paths = readPaths lines
    let rock = concatMap expandPath paths & nub
    let floor = rock & map snd & maximum
    let cave = zip rock (repeat '#') & Map.fromList :: Cave
    let caves = unfoldr (dropSand floor) (cave, [(-1, -1)])
    let final@(lstCave, _) =
            caves
            & dropWhile (\ (_, lastPath) -> (lastPath & last & snd) /= floor)
            & head
    let lastSize = lstCave & Map.size
    let restingSand = lastSize - Map.size cave
    -- display 200 (caves !! 32)
    display 1000 final
    print restingSand
    print $ lstCave & Map.toList & map snd & filter (== 'o') & length


dropSand :: Int -> (Cave, [Pos]) -> Maybe ((Cave, [Pos]), (Cave, [Pos]))
dropSand floor (cave, _) = do
    Just ((cave', lastPath), (cave', lastPath)) where
        lastPath = sandPath floor cave
        lastPos = last lastPath
        cave' = do
            if snd lastPos == floor then
                cave
            else
                Map.insert lastPos 'o' cave

sandPath floor cave =
    unfoldr advanceSand (500, 0) 
    where
        isAir pos = Map.member pos cave & not
        advanceSand (x, y) = do
            if y == floor then 
                Nothing 
            else
                [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
                    & find isAir
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

display :: Int -> (Cave, [Pos]) -> IO ()
display lns (cave, lstPath) = do
    forM_ (take lns gridCoords) (\ row -> map lookup row & putStrLn)
    putStrLn ""
    print ((minX, minY), (maxX, maxY))
    where 
        cave' = 
            cave & Map.insert (500, 0) '+' 
            & \ cave -> foldl (\ cv pos -> Map.insert pos '~' cv) cave lstPath
            & Map.insert (last lstPath) 'S'
        ps = Map.keys cave'
        minX = ps & map fst & minimum
        maxX = ps & map fst & maximum
        minY = ps & map snd & minimum
        maxY = ps & map snd & maximum

        xs = [0 .. (maxX - minX)]
        ys = [0 .. (maxY - minY)]

        gridCoords =
                ys
                & map (\ y -> xs & map (,y))
        lookup pos@(x, y) = do
            Map.lookup (minX + x, minY + y) cave'
                & \ case
                        (Just c) -> c
                        _ -> ' '
