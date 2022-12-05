module Day05 (solve) where

import Data.Function
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split

solve input lines = do
    let (stacks, instrs) = parse lines
    print $ unload stacks instrs reverse
    print $ unload stacks instrs id
       
unload stacks instrs transformLoad = do
    List.foldl move stacks instrs
        & tail
        & map head
    where
        move stacks inst = do
            let (qty, from, to) = inst
            let (fromStack, toStack) = (stacks !! from, stacks !! to)
            let newFrom = drop qty fromStack
            let newTo = transformLoad  (take qty fromStack) ++ toStack
            [ if n == from then newFrom else if n == to then newTo else stack 
                | (n, stack) <- zip [0..] stacks]


parse lines =
    (stacks, instrs) 
    where
        [top, bot]= Split.splitOn [""] lines
        stacks = do
            let asRows = List.transpose $ init top
            let numberedRows = zip [0..] asRows
            let crateRows = filter (\(i, _) -> i `mod` 4 == 1) numberedRows
            let zeroBased = map (dropWhile Char.isSpace . snd) crateRows
            []:zeroBased
        inst :: [String] -> (Int, Int, Int)
        inst strs = (read (strs !! 1), read $ strs !! 3, read $ strs !! 5)
        instrs = map (inst . Split.splitOn " ") bot
