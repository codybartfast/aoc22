module Day11 (solve) where

import Data.Function
import Data.List
import qualified Data.Map as Map
import qualified Data.List.Split as Split
-- import Data.List.NonEmpty (unfold)
-- import Data.Sequence (unfoldl)

solve input lines = do
    let monkies = readMonkies lines & zip [0..] & Map.fromList   
    let rounds = unfoldr (\ms -> Just (ms, playRound ms)) monkies
    print $ rounds !! 20 & Map.toList & map (count . snd) & sort & reverse & take 2 & product

playRound :: Map.Map Int Monkey -> Map.Map Int Monkey
playRound monkies = do
    foldl handleMonkey monkies [0 .. Map.size monkies - 1]

handleMonkey monkies n = do
    let Just monkey = Map.lookup n monkies
    let (newMonkey, throws) = monkeyDo monkey
    let monkies' = foldl catch monkies throws
    Map.insert n newMonkey monkies'

catch monkies (target, item) = do
    let Just monkey = Map.lookup target monkies
    Map.insert target (monkey {items = items monkey ++ [item]}) monkies

monkeyDo monkey = do
    let inspect item = do
            let worry = operation monkey item `div` 3
            (test monkey worry, worry)
    let throws = items monkey & map inspect
    (monkey {items = [], count = count monkey + length (items monkey)}, throws)

readMonkies lines =
    paras lines & map fromGroup
    where
        items :: String -> [Int]
        items line = 
            line 
            & Split.splitOn " " 
            & drop 4
            & map (read . dropWhileEnd (== ','))

        operation line =
            line 
            & Split.splitOn "= old " 
            & (!! 1)
            & Split.splitOn " "
            & \ [opTxt, valTxt] worry -> do
                let op = if opTxt == "+" then (+) else (*)
                let val = if valTxt == "old" then worry else read valTxt :: Int
                op worry val
        
        test :: [String] -> Int -> Int
        test lines = do
            let end line = line & Split.splitOn " " & last & read :: Int
            let denom = end (lines !! 3)
            let trueMonkey = end (lines !! 4)
            let falseMonkey = end (lines !! 5)
            \ worry -> if mod worry denom == 0 then trueMonkey else falseMonkey

        fromGroup group = Monkey 
            { items = group !! 1 & items
            , operation = group !! 2 & operation
            , test = test group
            , count = 0
            }

data Monkey = Monkey 
    { items :: [Int]
    , operation :: Int -> Int
    , test :: Int -> Int
    , count :: Int
    } 
instance Show Monkey where show m = show (count m) -- ++ " - " ++ show (note m)

paras = 
    paras [[]]
    where 
        paras :: [[String]] -> [String] -> [[String]]
        paras grps [] = grps & map reverse & reverse
        paras grps ("":lns) = paras ([]:grps) lns
        paras (grp:grps) (ln:lns) = paras ((ln:grp):grps) lns
