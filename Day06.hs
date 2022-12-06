module Day06 (solve) where

import Data.Function
import qualified Data.List as List

solve input lines = do
    print 
        $ fours input
        & map (length . List.nub)
        & takeWhile  (< 14)
        & length
        & (+ 14)

fours lst = result
    where
        fours' rslts (a:b:c:d:e:f:g:h:i:j:k:l:m:n:rest) = fours' ([a, b, c, d, e, f, g, h, i, j, k, l, m, n]:rslts) (b:c:d:e:f:g:h:i:j:k:l:m:n:rest)
        fours' rslts _ = rslts
        result = fours' [] lst & reverse