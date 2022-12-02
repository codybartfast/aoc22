
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split

import Day02 (solve)

trim = List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace

main = do
  fileContent <- readFile "input/Day02/Input.txt"
  let input = trim fileContent
  let lines = Split.splitOn "\n" input
  solve input lines
