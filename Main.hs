
import Data.List
import qualified Data.Char as Char
import qualified Data.List.Split as Split

import Day18 (solve)

trim = dropWhileEnd Char.isSpace

main = do
  fileContent <- readFile "input/Day18/Test01.txt"
  let input = trim fileContent
  let lines = Split.splitOn "\n" input
  solve input lines
  putStrLn ""
