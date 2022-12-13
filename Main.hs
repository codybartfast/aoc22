
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split

import Day13 (solve)

trim = List.dropWhileEnd Char.isSpace

main = do
  fileContent <- readFile "input/Day13/Input.txt"
  let input = trim fileContent
  let lines = Split.splitOn "\n" input
  solve input lines
  putStrLn ""
