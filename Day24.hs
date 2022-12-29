{-# LANGUAGE TupleSections #-}
module Day24 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.Set as Set

solve input lines = do
    let (pathOut, restOut) = explore initialValley entrance exit
    let lenOut = length pathOut
    print lenOut

    let (pathSnack, restSnack) = explore (restOut & head & fst) exit entrance
    let (pathFinal, _) = explore (restSnack & head & fst) entrance exit
    print $ lenOut + length pathSnack + length pathFinal

    where
        explore valley start end = do
            let time = iterate (minute width height) (valley, [start])
            let path = time & takeWhile 
                    ((> 0). minimum . map (distance end) . snd)
            let rest = drop (length path) time
            (path, rest)
        initialValley = readValley lines width height
        entrance = (1, 0)
        height = length lines - 2
        width = length (head lines) - 2
        exit = (width, height + 1)

minute width height (valley, positions) = do
    let valley' = nextValley width height valley
    let positions' = nextPos (occupied valley') positions
    (valley', positions')

nextValley width height valley = do
        let [wall, north, east, south, west] = valley
        [ wall
            , moveAll (moveNorth height) north
            , moveAll (moveEast width) east
            , moveAll (moveSouth height) south
            , moveAll (moveWest width) west ]

nextPos occupied positions =
    positions & concatMap nextPos & nub
    where
        nextPos pos@(x, y) =
            [(0, 0), (0, -1), (1, 0), (0, 1), (-1, 0)]
            & map (\ (dx, dy) -> (x + dx, y + dy))
            & filter (`Set.notMember` occupied)

distance (x', y') (x, y) = abs (x' - x) + abs (y' - y)
occupied valley = valley & concat & map fst & Set.fromList
moveNorth height (x, y) = y - 2 & (`mod` height) & (+ 1) & (x,)
moveEast width (x, y) = x & (`mod` width) & (+ 1) & (,y)
moveSouth height (x, y) = y & (`mod` height) & (+ 1) & (x,)
moveWest width (x, y) = x - 2 & (`mod` width) & (+ 1) & (,y)
moveAll move kind = map (\ (pos, c) -> (move pos, c)) kind

readValley lines width height = do
    let parts =
            zip [0 ..] lines & concatMap
                (\ (y, line) -> zipWith (\ x c -> ((x, y), c)) [0 .. ] line )
    let getKind k = parts & filter ((== k). snd)
    let wall = getKind '#'
    let north = getKind '^'
    let east = getKind '>'
    let south = getKind 'v'
    let west = getKind '<'
    [((width, height + 2), '#') : ((1, -1), '#') : wall
        , north, east, south, west]
