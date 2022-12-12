module Day12 (solve) where

import Data.Function
import Data.List
import Control.Monad (forM_)
import qualified Data.Char as Char


solve input lines = do
    let terrain = readTerrain lines
    -- showTerrain terrain
    print $ reachable terrain (terrain !! 1 !! 2)

reachable terrain square = do
    let height = length terrain
    let width = (length . head) terrain
    let get (x, y) = terrain !! y !! x
    let (pos@(x,y), (elevation, dist))  = square
    touching width height pos
        & map get
        & filter (\(_, (elv, _)) -> elv <= (elevation + 1))
        -- & map (\(pos, (elv, _)) -> (pos, (elv, elevation + 1)))
    where
        touching width height (x, y) = 
            [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
            & filter (\ (x, y) -> x >= 0 && x < width && y >= 0 && y < height)

showTerrain terrain = forM_ terrain $ \ row -> print row

readTerrain lines = do
    let values = map (map getValue) lines
    zipWith (\ y row -> zipWith (\ x val -> ((x, y), val)) [0..] row) [0..] values
    where
        getValue c  
            | c == 'S' = (0, 0)
            | c == 'E' = (25, 2000)
            | otherwise = (Char.ord c - Char.ord 'a', 2000)
        