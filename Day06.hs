module Day06 (solve) where

import Data.Function
import qualified Data.List as List

solve input lines = do
    print $ endOfFirstUniqueN 4 input
    print $ endOfFirstUniqueN 14 input

endOfFirstUniqueN n list =
    nwise n list
    & map (length . List.nub)
    & takeWhile  (< n)
    & length
    & (+ n)

nwise n list =
    List.tails list
    & map (List.take n)
    & List.dropWhileEnd ((< n). length)
