-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day17 (solve) where

import Data.Function ( (&) )
import Data.List
-- import qualified Data.Char as Char
-- import qualified Data.List.Split as Split
-- import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Bits as Set

solve input lines = do
    let wind = repeat input & concat
    -- let rock : rocks' = rocks
    let floor = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0)]
    let chamber = Set.fromList floor
    let full = foldl dropRock (chamber, wind) (take 2022 rocks)
    print $ full & fst & Set.toList & map snd & maximum

dropRock (chamber, wind) rock = do
    let top = Set.toList chamber & map snd & maximum
    let start = (2, top + 4)
    let (end, wind') = drop start wind
    let chamber' = foldl (flip Set.insert) chamber (rockLocs rock end)
    (chamber', wind')
    where
        clr = clear chamber rock
        move (loc@(x, y), _) gust = do
            let blownLoc = applyGust loc gust
            let loc'@(x', y') = if clr blownLoc then blownLoc else loc
            let fallLoc = (x', y' - 1)
            let canFall = clr fallLoc
            let loc'' = if canFall then fallLoc else loc'
            (loc'', canFall)
        drop loc (gust:wind) = do
                let (loc', fell) = move (loc, True) gust
                if fell
                    then drop loc' wind
                    else (loc', wind)
            
clear chamber rock loc@(x, y) =
    x >= 0
    && x + width rock -1 <= 6
    && doesnotOverlap ()
    where
        doesnotOverlap () =
            Set.intersection chamber (Set.fromList (rockLocs rock loc))
            & length
            & (== 0)

applyGust loc@(x, y) gust 
    | gust == '<' = (x - 1, y) 
    | gust == '>' = (x + 1, y)

rockLocs rock loc@(locX, locY) =
    map (\ (x, y) -> (locX + x, locY + y)) (parts rock)

rocks = repeat fiveRocks & concat

fiveRocks = 
    [ makeRock "-" [(0, 0), (1, 0), (2, 0), (3, 0)]
    , makeRock "+" [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
    , makeRock "J" [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
    , makeRock "l" [(0, 0), (0, 1), (0, 2), (0, 3)]
    , makeRock "o" [(0, 0), (0, 1), (1, 0), (1, 1)] ]

makeRock symbol parts = Rock
    { symbol = symbol
    , width = parts & map fst & maximum  & (+ 1)
    , height = parts & map snd & maximum & (+ 1)
    , parts = parts }

data Rock = Rock 
    { symbol :: String
    , width :: Int
    , height :: Int
    , parts :: [(Int, Int)] }

instance Show Rock where show r = show (symbol r)
