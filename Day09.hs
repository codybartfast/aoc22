module Day09 (solve) where

import Data.Function
import Data.List

solve input lines = do
    let headPath = lines & motions & moveHead (0, 0)
    print $ headPath & follow & nub & length
    print $ headPath & iterate follow & (!! 9) & nub & length

follow prev = do
    reverse $ follow [(0, 0)] prev
    where
        follow visited [] = visited
        follow visited@((x, y):_) ((px, py):prev') = do
            let (dx, dy) = (px - x, py - y)
            if (abs dx > 1) || abs dy > 1 then do
                let (ux, uy) = (unit dx, unit dy)
                follow ((x + ux, y + uy):visited) prev'
            else
                follow visited prev'
        unit 0 = 0
        unit n = n `div` abs n

moveHead start motions = do
    let move (x, y) motion =
            case motion of 'U' -> (x - 1, y)
                           'L' -> (x, y - 1)
                           'R' -> (x, y + 1)
                           'D' -> (x + 1, y)    
    scanl move start motions

motions lines = do
    concatMap expand lines
    where
        expand line = do
            let [[direction], n] = words line
            replicate (read n) direction
