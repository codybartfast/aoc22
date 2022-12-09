module Day09 (solve) where

import Data.Function
import Data.List

solve input lines = do
    let motions = parse lines
    let headPath = moveHead (0, 0) motions
    let tailPath = moveTail (0, 0) headPath
    print $ tailPath & nub & length

moveTail start headPath = do
    follow [start] headPath & reverse
    where
        unit 0 = 0
        unit n = n `div` abs n
        follow path [] = path
        follow path@((x, y):_) ((hx, hy):hrest) = do
            let (vx, vy) = (hx - x, hy - y)
            if (abs vx > 1) || abs vy > 1 then do
                let (ux, uy) = (unit vx, unit vy)
                follow ((x + ux, y + uy):path) hrest
            else
                follow path hrest


-- this could probably just be a foldMap but when is my poor haskells?
moveHead start motions =
    move [start] motions & reverse
    where
        move path [] = path
        move path@((x, y):_) (m:mrest) = do
            let next = case m of 'U' -> (x - 1, y)
                                 'L' -> (x, y - 1)
                                 'R' -> (x, y + 1)
                                 'D' -> (x + 1, y)
            move (next:path) mrest

parse lines = do
    concatMap expand lines
    where
        expand line = do
            let [[dir], n] = words line
            replicate (read n) dir
