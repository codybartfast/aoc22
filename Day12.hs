module Day12 (solve) where

import Data.Function ( (&) )
import Data.List
import Control.Monad (forM_)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List.Split as Split
import Data.List.NonEmpty (sortWith)

type Terrain = Map.Map (Int, Int) (Int, Int)

-- Elevations are inverted and routes are found from 'E', now the lowest point,
-- to 'S', now the highest point.  This is so that just one exploration is 
-- needed for Part Two where 'E' is known but the starting point is not.

solve input lines = do
    let coordCharPairs = getCoordCharPairs lines
    let terrain = readTerrain coordCharPairs
    let terrains = iterate explore terrain
    let finalTerrain =
            terrain
                & iterate explore
                & pairwise
                & find (uncurry (==))
                & (\ (Just (trn, _)) -> trn)

    let Just (_, part1) = finalTerrain & Map.lookup (findS coordCharPairs)
    print part1

    print
        $ findSorA coordCharPairs
        & map (readMap finalTerrain)
        & sortOn distance
        & head
        & distance
    
explore terrain = foldl exploreSquare terrain (Map.keys terrain)

exploreSquare :: Terrain -> (Int, Int) -> Terrain
exploreSquare terrain pos = do
    let square = readMap terrain pos
    let neighbourDist = distance square + 1
    square
        & reachable terrain
        & nowNearer neighbourDist
        & map (\ (pos, (elv, _)) -> (pos, (elv, neighbourDist)))
        & foldl (\ trn (pos, val) -> Map.insert pos val trn) terrain

nowNearer newDist neighbours =
    neighbours & filter (\ neighbour -> newDist < distance neighbour)

reachable terrain square =
    touching pos
        & map (readMap terrain)
        & filter (\(_, (elv, _)) -> elv <= (elevation + 1))
    where
        maxX = terrain & Map.keys & map fst & maximum
        maxY = terrain & Map.keys & map snd & maximum
        (pos@(x,y), (elevation, dist))  = square
        touching (x, y) = 
            [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
            & filter (\ (x, y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)
            
readMap terrain pos = do 
    let Just sqr = Map.lookup pos terrain
    (pos, sqr)

distance (pos, (elv, d)) = d

readTerrain coordCharPairs = do
    coordCharPairs
        & map (\ (pos, char) -> (pos, getValue char))
        & Map.fromList
    where
        getValue c  
            | c == 'E' = (0, 0)
            | c == 'S' = (25, 2000)
            | otherwise = (25 - (Char.ord c - Char.ord 'a'), 2000)

findS coordCharPairs = 
    coordCharPairs & find ((== 'S') . snd) & \ (Just (pos, _)) -> pos
        
findSorA coordsCharPairs = do
        coordsCharPairs
        & filter ((\ elv -> elv == 'S' || elv == 'a'). snd)
        & map fst

getCoordCharPairs lines = 
    lines
        & zipWith 
            (\ y row -> zipWith (\x val -> ((x, y), val)) [0 ..] row)
            [0..]     
        & concat

pairwise a = zip a (tail a)
