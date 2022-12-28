{-# LANGUAGE LambdaCase #-}
module Day23 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Crator = Set.Set (Int, Int)
type Dir = [(Int, Int)]

solve input lines = do
        let crator = readCrator lines
        let crators = iterate doRound (crator, dirs) & map fst
        let crator' = crators !! 10
        print $ freeSpace crator'
        print $ 
            zip crators (tail crators)
            & takeWhile (uncurry (/=))
            & length & (+ 1)
        
freeSpace crator = do
        let locs = crator & Set.toList
        let left = locs & map fst & minimum
        let right = locs & map fst & maximum
        let top = locs & map snd & minimum
        let bottom = locs & map snd & maximum
        (1 + right - left) * (1 + bottom - top) - length crator

doRound (crator, dirs) = do
    let crator' =
            nextLocs
                & filter (uncurry (/=))
                & foldl addPlan Map.empty
                & Map.toList
                & filter ((== 1) . length . snd)
                & foldl execMove crator
    (crator', tail dirs ++ [head dirs])
    where
        execMove crator (dest, [orig]) =
            crator & Set.delete orig & Set.insert dest
        addPlan plan (elf, elf') =
            case Map.lookup elf' plan of
                Nothing -> Map.insert elf' [elf] plan
                (Just elves) -> Map.insert elf' (elf : elves) plan
                
        nextLocs = 
            crator
            & Set.toList
            & map (\ elf -> (elf, nextLoc elf))
        nextLoc elf =
            if nhoodClear elf then elf else
                dirs
                & map (moveDir elf)
                & filter (/= Nothing)
                & \ case [] -> elf; (Just loc) : _ -> loc
        moveDir elf@(x, y) dir@[_, (dx, dy), _] =
            if allClear elf dir then Just (x + dx, y + dy) else Nothing
        allClear elf@(x, y) deltas =
            deltas
            & map (\ (dx, dy) -> (x + dx, y + dy))
            & filter (`Set.member` crator)
            & (== [])
        nhoodClear elf = allClear elf neighbourhood

north = [(-1, -1), (0, -1), (1, -1)]
south = [(-1, 1), (0, 1), (1, 1)]
west = [(-1, -1), (-1, 0), (-1, 1)]
east = [(1, -1), (1, 0), (1, 1)]
dirs = [north, south, west, east]
neighbourhood = concat dirs & nub

readCrator lines =
    zip [0 ..] lines & concatMap
        (\ (y, line) -> zip [0 ..] line & map (\ (x, c) -> ((x, y), c)))
    & filter ((== '#') . snd)
    & map fst
    & Set.fromList