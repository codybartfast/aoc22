module Day03 (solve) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set

solve input lines = do
    let commonItems = map packCommon lines
    print $ sum $ map priority commonItems

    let groups = group [] lines
    print $ sum $ map (priority . groupCommon) groups

packCommon line = do
    let packSize = List.length line `div` 2
    let items1 = Set.fromList (take packSize line)
    let items2 = Set.fromList (drop packSize line)
    pick $ Set.intersection items1 items2

pick = head . Set.toList

priority c = do
    let code = Char.ord c
    if code < 97 then code - 38 else code - 96

group :: [[String]] -> [String] -> [[String]]
group groups [] = groups
group groups elves = group (take 3 elves:groups) (drop 3 elves)

groupCommon group =
    pick $ List.foldl1 Set.intersection (map Set.fromList group)
