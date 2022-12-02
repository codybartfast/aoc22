module Day01 (solve) where

import qualified Data.List as List

group :: [[String]] -> [String] -> [[String]]
group grps [] = grps
group [] (ln:lns) = group [[ln]] lns
group grps ("":lns) = group ([]:grps) lns
group (grp:grps) (ln:lns) = group ((ln:grp):grps) lns

solve input lines = do
  let grpStrs = group [] lines
  let grps = map (map read) grpStrs :: [[Int]]
  let sums = map sum grps
  let sorted = List.reverse $ List.sort sums

  print (head sorted)
  print (sum $ take 3 sorted)
