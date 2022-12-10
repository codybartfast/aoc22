module Day10 (solve) where

import Data.Function
import Data.List

solve input lines = do
    let program = map readLine lines
    let states = scanl execute start program
    let max = states & last & fst
    let times = [0, 40 .. 200] & map (+ 20) 
    let atTime n = states & takeWhile (\ (t, _) -> t < n) & last & (\ (t, v) -> (n, v))
    let calc (t, n) = t * n
    print $ times & map atTime & map calc & sum

execute :: (Int, Int) -> Maybe Int -> (Int, Int)
execute (time, x) Nothing = (time + 1, x)
execute (time, x) (Just v) = (time + 2, x + v)


readLine :: String -> Maybe Int
readLine line = do
    let (first:rest) = words line
    if first == "noop" then Nothing else Just $ rest & head & read

start = (0, 1)