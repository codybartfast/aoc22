module Day06 (solve) where

import Data.Function
import qualified Data.List as List

solve input lines = do
    print 
        $ fours input
        & map (length . List.nub)
        & takeWhile  (< 4)
        & length
        & (+ 4)

fours lst = result
    where
        fours' rslts (a:b:c:d:rest) = fours' ([a, b, c, d]:rslts) (b:c:d:rest)
        fours' rslts _ = rslts
        result = fours' [] lst & reverse