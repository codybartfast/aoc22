module Day12 (solve) where

import Data.Function
import Data.List
import Control.Monad (forM_)
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.List.Split as Split

type Terrain = Map.Map (Int, Int) (Int, Int)

solve input lines = do
    let terrain = readTerrain lines
    showTerrain terrain
    print $ reachable terrain (get terrain (2, 1))

reachable terrain square = do
    touching pos
        & map (get terrain)
        & filter (\(_, (elv, _)) -> elv <= (elevation + 1))
    where
        maxX = terrain & Map.keys & map fst & maximum
        maxY = terrain & Map.keys & map snd & maximum
        (pos@(x,y), (elevation, dist))  = square
        touching (x, y) = 
            [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
            & filter (\ (x, y) -> x >= 0 && x <= maxX && y >= 0 && y <= maxY)
            
showTerrain ::  Terrain -> IO ()
showTerrain terrain = do
    let grid = terrain & Map.toList
    let height = grid & map (snd . fst) & maximum & (+ 1) 
    let cols = Split.chunksOf height grid
    forM_ (transpose cols) $ \ x -> print x

get :: Terrain -> (Int, Int) -> ((Int, Int), (Int, Int))
get terrain pos = do 
    let Just sqr = Map.lookup pos terrain
    (pos, sqr)

readTerrain :: [String] -> Terrain
readTerrain lines = do
    let values = map (map getValue) lines
    zipWith (\ y row -> 
            zipWith (\ x val -> ((x, y), val)) [0..] row) [0..] values
        & concat
        & Map.fromList
    where
        getValue c  
            | c == 'S' = (0, 0)
            | c == 'E' = (25, 2000)
            | otherwise = (Char.ord c - Char.ord 'a', 2000)
        