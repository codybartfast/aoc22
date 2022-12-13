module Day11 (solve) where

import Data.Function
import Data.List
import qualified Data.Map as Map
import qualified Data.List.Split as Split

solve input lines = do
    let monkies = readMonkies lines & zip [0..] & Map.fromList
    let productOfDivisors =
            monkies 
            & Map.toList & map (divisor . snd) 
            & product
    let manageWorry1 = (`div` 3)
    let manageWorry2 = (`mod` productOfDivisors)
    print $ monkeyBusiness monkies 20 manageWorry1
    print $ monkeyBusiness monkies 10000 manageWorry2

monkeyBusiness monkies numRounds manageWorry = do
    let rounds = unfoldr (\ms -> Just (ms, playRound manageWorry ms)) monkies
    rounds !! numRounds 
        & Map.toList & map (count . snd) 
        & sort & reverse & take 2 & product

playRound extra monkies = do
    foldl (handleMonkey extra) monkies [0 .. Map.size monkies - 1]

handleMonkey extra monkies n = do
    let Just monkey = Map.lookup n monkies
    let (newMonkey, throws) = monkeyDo extra monkey
    let monkies' = foldl catch monkies throws
    Map.insert n newMonkey monkies'

catch monkies (target, item) = do
    let Just monkey = Map.lookup target monkies
    Map.insert target (monkey {items = items monkey ++ [item]}) monkies

monkeyDo extra monkey = do
    let inspect item = do
            let worry = operation monkey item & extra
            (test monkey worry, worry)
    let throws = items monkey & map inspect
    (monkey {items = [], count = count monkey + length (items monkey)}, throws)

readMonkies lines =
    paras lines & map fromGroup
    where
        end line = line & Split.splitOn " " & last & read :: Int

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

        divisor lines = end (lines !! 3)

        test :: [String] -> Int -> Int
        test lines = do
            let divisor = end (lines !! 3)
            let trueMonkey = end (lines !! 4)
            let falseMonkey = end (lines !! 5)
            \ worry -> if mod worry divisor == 0 then trueMonkey else falseMonkey

        fromGroup group = Monkey 
            { items = group !! 1 & items
            , operation = group !! 2 & operation
            , divisor = divisor group
            , test = test group
            , count = 0
            }

data Monkey = Monkey 
    { items :: [Int]
    , operation :: Int -> Int
    , divisor :: Int
    , test :: Int -> Int
    , count :: Int
    } 
instance Show Monkey where show m = show (count m) ++ " - " ++ show (items m)

paras = 
    paras [[]]
    where 
        paras :: [[String]] -> [String] -> [[String]]
        paras grps [] = grps & map reverse & reverse
        paras grps ("":lns) = paras ([]:grps) lns
        paras (grp:grps) (ln:lns) = paras ((ln:grp):grps) lns
