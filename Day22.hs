{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day22 (solve) where

import Data.Function ( (&) )
import Data.List
-- import qualified Data.Char as Char
import qualified Data.List.Split as Split
-- import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char

solve input lines = do
    let (grove, instrs) = readGrove lines
    let start = ((rows grove & head & fst, 0), 0)
    let segments = foldl (addSegment grove) [[start]] instrs
    let final = segments & head & last
    print $ final & password
    -- print start

password ((x, y), f) = 1000 * (y + 1) + 4 * (x + 1) + f

addSegment grove segments instr = do
    let pos = segments & head & last
    let newSeg = followInstr pos instr
    newSeg : segments
    where
        followInstr pos@(coord, dir) instr@(turn, dist) = do
            let dir' = (dir + turn)  `mod` 4
            advance (coord, dir') dist
        advance pos n = unfoldr tryAdvanceOne pos & take (n + 1)
        tryAdvanceOne pos =
            if isWall pos then Nothing else Just (pos, nextPos grove pos)
        isWall (coord, dir) = Set.member coord (walls grove)
        nextPos grove ((x, y), dir) =
            case dir of
                0 -> ((x + 1, y), dir) & wrapX
                1 -> ((x, y + 1), dir) & wrapY
                2 -> ((x - 1, y), dir) & wrapX
                3 -> ((x, y - 1), dir) & wrapY
                _ -> error (show dir)
            where
                wrapX ((x, y), dir) = do
                    let (start, end) = rows grove !! y
                    let x' = (x - start) `mod` (end - start) & (+ start)
                    ((x', y), dir)
                wrapY ((x, y), dir) = do
                    let (start, end) = cols grove !! x
                    let y' = (y - start) `mod` (end - start) & (+ start)
                    ((x, y'), dir)

data Grove = Grove 
    { rows :: [(Int, Int)]
    , cols :: [(Int, Int)]
    , walls :: Set.Set (Int, Int)
    } deriving (Show)

readGrove lines = do
    let route = lines !! (gap + 1)
    let rows = map tiles groveTxt
    let cols = map tiles (transpose groveTxt)
    let grove = Grove 
            { rows = rows
            , cols = cols
            , walls = walls }
    let instrs = getInstrs ('F' : lines !! (gap + 1))
    (grove, instrs)
    where
        Just gap = elemIndex "" lines
        groveTxt = 
            lines & take gap
            & map (\ line -> (line ++ repeat ' ') & take width)
        height = length groveTxt
        width = length $ map length groveTxt
        tiles line =
            (length $ takeWhile (== ' ') line
            , length $ dropWhileEnd (== ' ') line)
        walls = 
            [0 .. height - 1] & concatMap (\ y ->
                [0 .. width - 1] & map (,y))    
            & filter (\ (x, y) -> groveTxt !! y !! x == '#')
            & Set.fromList
        getInstr instrTxt = do
            let t = head instrTxt
            let ds = takeWhile Char.isNumber (tail instrTxt)
            let chunk = t : ds
            let turn = case t of 'L' -> -1; 'R' -> 1; 'F' -> 0
            ((turn, read ds :: Int), drop (length chunk) instrTxt)
        getInstrs [] = []
        getInstrs routeTxt = do
            let (chunk, rest) = getInstr routeTxt
            chunk : getInstrs rest
