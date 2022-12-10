module Day10 (solve) where

import Data.Function
import Data.List
import qualified Data.List.Split as Split

solve input lines = do
    let program = map readLine lines
    let states = scanl execute (0, 1) program
    let values = getValues states
    let cycleValue c = values !! (c - 1)
    let interesting (c, n) = c * n
    print $ map (interesting . cycleValue) [20, 60 .. 220] & sum
    putStrLn $ 
        values 
        & map (\ (c, v) -> if abs ((c - 1) `mod` 40 - v) < 2 then '#' else ' ')
        & Split.chunksOf 40
        & concatMap ('\n' :)

getValues states = do
    scanl 
        (\ states' c -> dropWhile (\ (c', _) -> c' < c ) states')
        states
        [0 .. 240]
    & map (snd . head)
    & zip [1..]
    
execute :: (Int, Int) -> Maybe Int -> (Int, Int)
execute (cycle, x) Nothing = (cycle + 1, x)
execute (cycle, x) (Just v) = (cycle + 2, x + v)

readLine :: String -> Maybe Int
readLine line = do
    let (first:rest) = words line
    if first == "noop" then Nothing else Just $ rest & head & read
