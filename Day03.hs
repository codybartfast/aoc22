module Day03 (solve) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Set as Set

solve input lines = do
    print (take 200 input)
    putStrLn ""
    let commonItems = map findCommon lines
    -- print commonItems
    print (sum $ map priority commonItems)

findCommon line = do
    let packSize = List.length line `div` 2
    let items1 = Set.fromList (take packSize line)
    let items2 = Set.fromList (drop packSize line)
    head $ Set.toList $ Set.intersection items1 items2

priority c = do
    let code = Char.ord c
    if code < 97 then code - 38 else code - 96

