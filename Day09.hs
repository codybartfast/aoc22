module Day09 (solve) where

import Data.Function
import Data.List

solve input lines = do
    let headPath = lines & motions & moveHead start
    print $ headPath & follow start & nub & length
    print $ headPath & iterate (follow start) & (!! 9) & nub & length

follow start prev = do
    scanl follow start prev
    where
        follow (x, y) (px, py) = do
            let (dx, dy) = (px - x, py - y)
            if (abs dx > 1) || abs dy > 1 then
                (x + unit dx, y + unit dy)
            else
                (x, y)
        unit n | n == 0 = 0 | otherwise = n `div` abs n

moveHead start motions = do
    let move (x, y) motion =
            case motion of 'U' -> (x, y - 1)
                           'L' -> (x -1, y)
                           'R' -> (x + 1, y)
                           'D' -> (x, y + 1)    
    scanl move start motions

motions lines = do
    concatMap expand lines
    where
        expand line = do
            let [[direction], count] = words line
            replicate (read count) direction

start = (0, 0)
