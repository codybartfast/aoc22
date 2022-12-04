module Day03 (solve) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split

solve input lines = do
    let commonItems = map packCommon lines
    print $ sum $ map priority commonItems

    let groups = Split.chunksOf 3 lines
    print $ sum $ map (priority . groupCommon) groups

packCommon line = do
    let packSize = List.length line `div` 2
    head $ List.intersect (take packSize line) (drop packSize line)

priority c = do
    let code = Char.ord c
    if code < 97 then code - 38 else code - 96

groupCommon group = head $ List.foldl1 List.intersect group
