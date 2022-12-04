module Day04 (solve) where

import qualified Data.List.Split as Split

parseLine line = do
    let [left,right] = Split.splitOn "," line
    (toRange left, toRange right)

toRange :: String -> (Int, Int)
toRange str = do
    let [first, last] = Split.splitOn "-" str
    (read first, read last)

contains a b = fst a <= fst b && snd a >= snd b
startsIn a b = fst a >= fst b && fst a <= snd b
oneOf pred (a, b) = pred a b || pred b a

solve input lines = do   
    let pairs = map parseLine lines
    print $ length $ filter (oneOf contains) pairs
    print $ length $ filter (oneOf startsIn) pairs
