import Data.List
import qualified Data.Char as Char
import qualified Data.List.Split as Split

import Day25 (solve)

trim = dropWhileEnd Char.isSpace

main = do
  fileContent <- readFile "input/2022/Day25/Input.txt"
  let input = trim fileContent
  let lines = Split.splitOn "\n" input
  solve input lines
  putStrLn ""
