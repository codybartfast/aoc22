module Day06 (solve) where

import Data.Function
import Data.List

solve input lines = do
    print $ endOfFirstUniqueN 4 input
    print $ endOfFirstUniqueN 14 input

endOfFirstUniqueN n list =
    nwise n list
    & map (length . nub)
    & takeWhile  (< n)
    & length
    & (+ n)

nwise n list =
    tails list
    & map (take n)
    & dropWhileEnd ((< n). length)
