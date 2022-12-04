
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.List.Split as Split

import Day05 (solve)

trim = List.dropWhileEnd Char.isSpace . dropWhile Char.isSpace

main = do
  fileContent <- readFile "input/Day05/Test01.txt"
  let input = trim fileContent
  let lines = Split.splitOn "\n" input
  solve input lines
  putStrLn ""