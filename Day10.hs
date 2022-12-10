module Day10 (solve) where

import Data.Function
import Data.List

solve input lines = do
    let program = map readLine lines
    let states = scanl execute start program
    let values = getValues states
    let valueAtTime n = values !! (n - 1)
    let calc (t, n) = t * n
    print $  map (calc .valueAtTime) [20, 60 .. 220] & sum
        
getValues states = do
    scanl 
        (\ states' t -> skipWhile (\ (t', _) -> t' < t ) states')
        states
        [0 .. 220]
    & map (snd . head)
    & zip [1..]
    
execute :: (Int, Int) -> Maybe Int -> (Int, Int)
execute (time, x) Nothing = (time + 1, x)
execute (time, x) (Just v) = (time + 2, x + v)

readLine :: String -> Maybe Int
readLine line = do
    let (first:rest) = words line
    if first == "noop" then Nothing else Just $ rest & head & read

skipWhile :: (a -> Bool) -> [a] -> [a]
skipWhile _ [] = []
skipWhile pred list@(hd:tl) = do
    if pred hd then skipWhile pred tl else list

start = (0, 1)
