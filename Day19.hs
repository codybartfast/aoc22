{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day19 (solve) where

import Data.Function ( (&) )
import Data.List
-- import qualified Data.Char as Char
import qualified Data.List.Split as Split
import Control.Applicative (Alternative(empty))
-- import qualified Data.Map as Map

type Inventory = [Int]
type Army = [Int]

solve input lines = do
    let blueprints = readBlueprints lines
    let inventory = [0, 0, 0, 0]
    let army = [1, 0, 0, 0]
    let costs = blueprints & head & snd
    let adv = prune . advanceMinute costs
    -- print $ length costs
    print $ [(inventory, army)] 
        & iterate adv 
        & (!! 3)
        -- & map fst
        & map (uncurry (flip zip))
        -- & find ((== (1, 2)) . (!! 0))

prune :: [([Int], [Int])] ->  [([Int], [Int])]
prune states =
    states
    & sortOnDesc value
    & take 400

value (inventory, army) = do
        let factors = map (2^) [0 .. ]
        sum $ zipWith (*) factors (army ++ [last inventory])

advanceMinute :: [[Int]] -> [([Int], [Int])] -> [([Int], [Int])]
advanceMinute costs states = do
    let states' = map (\ (a, b) -> (b, (a, b))) states
    let states'' = spendChoices costs states'
    map 
        (\ (origArmy, (inventory, army)) -> (zipWith (+) inventory origArmy, army))
        states''

spendChoices costs states = do
    let x = unfoldr (\ sts -> do
                let sts' = concatMap (tryBuildEach costs) sts  
                Just (sts, sts')) states

    takeWhile (/= []) x
    & concat & nub
    -- let x = concatMap (tryBuildEach costs) [(inventory, army)]
    -- concatMap (tryBuildEach costs) x & nub


tryBuildEach :: (Num a, Ord a) => [[a]] -> ([Int], ([a], [Int])) -> [([Int], ([a], [Int]))]
tryBuildEach costs (origArmy, (inventory, army)) = do
    costs
        & map (zipWith (-) inventory)
        & zip [0 .. ]
        & filter (all (>= 0) . snd)
        & map (\ (kind, inventory') -> (origArmy, (inventory', incAt kind army)))

incAt :: Int -> [Int] -> [Int]
incAt pos = 
    zipWith (\ i n -> if i == pos then n + 1 else n) [0 .. ]
    
readBlueprints lines = do
    map readLine lines
    where
        readLine line = do
            let parts = Split.splitOn " " line
            let id = parts !! 1 & init & read  :: Int
            let nums = [6, 12, 18, 21, 27, 30] & map (read . (parts !!)) :: [Int]
            let details =
                    [ [head nums, 0, 0, 0],
                      [nums !! 1, 0, 0, 0],
                      [nums !! 2, nums !! 3, 0, 0],
                      [nums !! 4, 0, nums !! 5, 0] ]
            (id, details)
            
sortOnDesc pred = reverse . sortOn pred
-- sortDesc = reverse . sort
