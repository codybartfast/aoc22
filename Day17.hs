module Day17 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.Set as Set

solve input lines = do
    let wind = repeat input & concat
    let floor = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0)]
    let chamber = Set.fromList floor
    let results = scanl dropRock (chamber, wind) rocks

    let ycounts = zip [0..] (map maxCount results)
    let [(cycle, (height, _)), (cycle', (height', _)), (cycle'', (height'', _))]
            = filter ((== 7) . snd . snd) ycounts & take 3
    let cycleOff = cycle' - cycle
    let heightOff = height' - height
    let cycleMod = cycle'' - cycle'
    let heightMod = height'' - height'
    let calculateHeight targetCycles = do
            let fullCycles = (targetCycles - cycleOff ) `div` cycleMod
            let cycleBase = cycleOff + (fullCycles * cycleMod)
            let heightBase = heightOff + (fullCycles * heightMod)
            let cyclesRem = targetCycles - cycleBase
            let heightRem = do
                    measureHeight (results !! (cycleOff + cyclesRem))
                        - measureHeight (results !! cycleOff)
            heightBase + heightRem

    print $ calculateHeight 2022
    print $ calculateHeight 1000000000000
    
maxCount (chamber, _) = do
    let ys = Set.toList chamber & map snd
    let mx = maximum ys
    (mx, filter (== mx) ys & length)

measureHeight (chamber, _) = chamber & Set.toList & map snd & maximum

dropRock (chamber, wind) rock = do
    let top = Set.toList chamber & map snd & maximum
    let start = (2, top + 4)
    let (end, wind') = drop start wind
    let chamber' = foldl (flip Set.insert) chamber (rockLocs rock end)
    (chamber', wind')
    where
        clr = clear chamber rock
        move (loc@(x, y), _) gust = do
            let blownLoc = blow loc gust
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
    x >= 0 && x + width rock -1 <= 6 && doesnotOverlap ()
    where
        doesnotOverlap () =
            Set.intersection chamber (Set.fromList (rockLocs rock loc))
            & length
            & (== 0)

blow loc@(x, y) gust | gust == '<' = (x - 1, y) | gust == '>' = (x + 1, y)

rockLocs rock loc@(locX, locY) =
    map (\ (x, y) -> (locX + x, locY + y)) (parts rock)

rocks = do
    let fiveRocks = 
            [ makeRock "-" [(0, 0), (1, 0), (2, 0), (3, 0)]
            , makeRock "+" [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
            , makeRock "J" [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)]
            , makeRock "l" [(0, 0), (0, 1), (0, 2), (0, 3)]
            , makeRock "o" [(0, 0), (0, 1), (1, 0), (1, 1)] ]
    repeat fiveRocks & concat

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
