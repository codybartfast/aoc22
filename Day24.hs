{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day24 (solve) where

import Data.Function ( (&) )
import Data.List
-- import qualified Data.Char as Char
-- import qualified Data.List.Split as Split
-- import qualified Data.Map as Map
import qualified Data.Set as Set

solve input lines = do
    let start = (1, 0)
    let height = length lines - 2
    let width = length (head lines) - 2
    let end = (width, height + 1)
    let dist = distance end
    let kinds@[wall, north, east, south, west] = readValley lines
    let time = iterate ( minute width height) (kinds, [start])
    print $
        time & tail 
        & takeWhile ((> 0). minimum . map dist . snd) 
        & length
        

minute width height (kinds, positions) = do
    let positions' = nextPos (occupied kinds) positions
    let [wall, north, east, south, west] = kinds
    let kinds' =
            [ wall
            , moveAll (moveNorth height) north
            , moveAll (moveEast width) east
            , moveAll (moveSouth height) south
            , moveAll (moveWest width) west ]
    (kinds', positions')

nextPos occupied positions =
    positions & concatMap nextPos & nub
    where
        nextPos pos@(x, y) = 
            [(0, 0), (0, -1), (1, 0), (0, 1), (-1, 0)]
            & map (\ (dx, dy) -> (x + dx, y + dy))
            & filter (`Set.notMember` occupied)

distance (x', y') (x, y) = abs (x' - x) + abs (y' - y)
occupied kinds = kinds & concat & map fst & Set.fromList
moveNorth height (x, y) = y - 2 & (`mod` height) & (+ 1) & (x,)
moveEast width (x, y) = x & (`mod` width) & (+ 1) & (,y)
moveSouth height (x, y) = y & (`mod` height) & (+ 1) & (x,)
moveWest width (x, y) = x - 2 & (`mod` width) & (+ 1) & (,y)
moveAll move kind = map (\ (pos, c) -> (move pos, c)) kind

readValley lines = do
    let parts =
            zip [0 ..] lines & concatMap
                (\ (y, line) -> zipWith (\ x c -> ((x, y), c)) [0 .. ] line )
    let getKind k = parts & filter ((== k). snd)
    let wall = getKind '#'
    let north = getKind '^'
    let east = getKind '>'
    let south = getKind 'v'
    let west = getKind '<'
    [((1, -1), '#'): wall, north, east, south, west]