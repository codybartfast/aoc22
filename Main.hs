
import Data.List
import qualified Data.Char as Char
import qualified Data.List.Split as Split

import Day17 (solve)

trim = dropWhileEnd Char.isSpace

main = do
  fileContent <- readFile "input/Day17/Input.txt"
  let input = trim fileContent
  let lines = Split.splitOn "\n" input
  solve input lines
  putStrLn ""
