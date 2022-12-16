-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day16 (solve) where

import Data.Function ( (&) )
import Data.List
-- import qualified Data.Char as Char
import qualified Data.List.Split as Split
-- import qualified Data.Map as Map

data Valve = 
    Valve {name :: String, rate :: Int, leads :: [String]} deriving (Show)

solve input lines = do
    print $ readValves lines

readValves lines = do
    map readValve lines 
    where
        readValve line = do
            let parts = Split.splitOn " " line
            let id = parts !! 1
            let rate = 
                    parts !! 4 & Split.splitOn "=" & (!! 1) & init & read :: Int
            let reachable = drop 9 parts & map (dropWhileEnd (== ','))
            Valve {name = id, rate = rate, leads = reachable}
