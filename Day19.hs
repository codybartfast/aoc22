{-# LANGUAGE TupleSections #-}
module Day19 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.List.Split as Split

solve input lines = do
    let blueprints = readBlueprints lines

    print $
        blueprints
        & map (uncurry (*) . quality 24)
        & sum

    print $ 
        blueprints 
        & take 3 
        & map (snd . quality 32) 
        & product

quality minutes blueprint = do
    let inventory = [0, 0, 0, 0]
    let army = [1, 0, 0, 0]
    let (id, costs) = blueprint
    [(inventory, army)] 
        & iterate (prune . advanceMinute costs) 
        & (!! minutes)
        & geodes
        & (id,)
    where
        geodes states = states & map (last . fst) & maximum
        prune = nub . take 1024 . sortOnDesc value 

value (inventory, army) =
        sum (zipWith (*) inventory [1, 20, 400, 8000])
        + sum (zipWith (*) army [2, 40, 800, 16000])

advanceMinute costs states =
    states
        & map (\ (a, b) -> (b, (a, b))) 
        & spendChoices costs
        & map (\ (origArmy, (inventory, army)) -> 
            (zipWith (+) inventory origArmy, army))

spendChoices costs states =  states ++ concatMap (tryBuildEach costs) states

tryBuildEach costs (origArmy, (inventory, army)) =
    costs
        & map (zipWith (-) inventory)
        & zip [0 .. ]
        & filter (all (>= 0) . snd)
        & map (\ (kind, inventory') -> 
            (origArmy, (inventory', incAt kind army)))

incAt pos = zipWith (\ i n -> if i == pos then n + 1 else n) [0 .. ]

sortOnDesc pred = reverse . sortOn pred

readBlueprints =
    map readLine
    where
        readLine line = do
            let parts = Split.splitOn " " line
            let id = parts !! 1 & init & read  :: Int
            let nums = 
                    [6, 12, 18, 21, 27, 30] & map (read . (parts !!)) :: [Int]
            let details =
                    [ [head nums, 0, 0, 0],
                      [nums !! 1, 0, 0, 0],
                      [nums !! 2, nums !! 3, 0, 0],
                      [nums !! 4, 0, nums !! 5, 0] ]
            (id, details)
