{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Day14 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.Map as Map
import qualified Data.List.Split as Split
import Control.Monad (forM_)

solve input lines = do
    let paths = readPaths lines
    let (restingSand, lstCave, lstPath) = poorSand paths

    let floor = paths & concat & map snd & maximum & (+ 2)
    let paths' = [(500 - floor * 2, floor), (500 + floor * 2, floor)] : paths
    let (restingSand', lstCave', lstPath') = poorSand paths'
    -- display lstCave lstPath
    print restingSand
    print restingSand'

poorSand paths = do
    let abyss = paths & concat & map snd & maximum & (+ 2)
    let rock = concatMap expandPath paths & nub
    let cave = zip rock (repeat '#') & Map.fromList
    let caves = unfoldr (dropSandUnit abyss) (cave, [(-1, -1)])    
    let final@(lstCave, lstPath) =
            caves
            & dropWhile (\ (_, lastPath) -> do
                let depth = lastPath & last & snd
                depth /= abyss && depth /= 0)
            & head
    let lastSize = lstCave & Map.size
    let restingSand = lastSize - Map.size cave
    (restingSand, lstCave, lstPath)

dropSandUnit abyss (cave, _) = do
    Just ((cave', lastPath), (cave', lastPath)) where
        lastPath = sandPath abyss cave
        lastPos = last lastPath
        cave' = do
            if snd lastPos == abyss then
                cave
            else
                Map.insert lastPos 'o' cave

sandPath abyss cave =
    unfoldr advanceSand (500, -1) 
    where
        isAir pos = Map.member pos cave & not
        advanceSand (x, y) = do
            if y == abyss then 
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
            | x1 == x2 = map (x1,) [min y1 y2 .. max y1 y2]
            | y1 == y2 = map (, y1) [min x1 x2 .. max x1 x2]
        pairwise a = zip a (tail a)

readPaths lines = do
    let sectionTxts = map (Split.splitOn " -> ") lines
    sectionTxts 
        & map (map (\ xyTxt -> do
            let [xTxt, yTxt] = Split.splitOn "," xyTxt
            (read xTxt, read yTxt) )) 

display cave lstPath = do
    forM_ gridCoords (\ row -> map lookup row & putStrLn)
    where 
        cave' =
            cave 
            & \ cave -> foldl (\ cv pos -> Map.insert pos '~' cv) cave lstPath
            & Map.insert (500, 0) '+' 
        ps = Map.keys cave'
        minX = ps & map fst & minimum
        maxX = ps & map fst & maximum
        minY = ps & map snd & minimum
        maxY = ps & map snd & maximum

        xs = [0 .. (maxX - minX)]
        ys = [0 .. (maxY - minY)]

        gridCoords = ys & map (\ y -> xs & map (,y))
        lookup pos@(x, y) = do
            Map.lookup (minX + x, minY + y) cave'
                & \ case
                        (Just c) -> c
                        _ -> ' '
