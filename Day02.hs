module Day02 (solve) where

import qualified Data.Char as Char

norm n = (n + 3) `mod` 3

score (left, right) = right + 1 + 3 * norm (right - left + 1)

select (left, right) = (left, norm (left + (right - 1)))

solve input lines = do
  let c2i = Char.ord
  let lineToPair (left:_:right:_) = (c2i left - c2i 'A', c2i right - c2i 'X')
  let pairs = map lineToPair lines
  print $ sum $ map score pairs
  print $ sum $ map (score . select) pairs
