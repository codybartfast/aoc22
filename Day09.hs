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
            let (vx, vy) = (px - x, py - y)
            if (abs vx > 1) || abs vy > 1 then do
                let (ux, uy) = (unit vx, unit vy)
                follow ((x + ux, y + uy):visited) prev'
            else
                follow visited prev'
        unit 0 = 0
        unit n = n `div` abs n

-- this could probably just be a foldMap but when is my poor haskells?
moveHead start motions =
    move [(0, 0)] motions & reverse
    where
        move visited [] = visited
        move visited@((x, y):_) (motion:motions') = do
            let next = case motion of 'U' -> (x - 1, y)
                                      'L' -> (x, y - 1)
                                      'R' -> (x, y + 1)
                                      'D' -> (x + 1, y)
            move (next:visited) motions'

motions lines = do
    concatMap expand lines
    where
        expand line = do
            let [[dir], n] = words line
            replicate (read n) dir
