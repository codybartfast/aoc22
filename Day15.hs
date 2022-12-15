-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day15 (solve) where

import Data.Function ( (&) )
import Data.List 
import qualified Data.Char as Char
import qualified Data.List.Split as Split
-- import qualified Data.Map as Map

type Report = [((Int, Int), (Int, Int))]

solve input lines = do
    let report = readReport lines
    let row = if length report == 14 then 10 else 2000000
    print $ 
        report
        & cannotContain row

cannotContain row report = do
    let ((minX, _), (maxX, _)) = reportRange report
    let beacons = report & map snd
    let beaconsInRow = beacons & filter ((== row) . snd) & nub
    let scanned = [minX .. maxX] & filter inRangeAny
    length scanned - length  beaconsInRow
    where
        inRangeAny x =
            report
                & any (inRange (x, row))
        inRange pos reading =
            dist pos (fst reading) <= range reading
        dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

reportRange report = do
    report
        & map scannerRange
        & (\ ranges -> (
            (minimum $ map (fst . fst) ranges,
                minimum $ map (snd . fst) ranges),
            (maximum $ map (fst . snd) ranges,
                maximum $ map (snd . snd) ranges)))
        where
            scannerRange reading@((sx,  sy), (bx,  by)) = do
                let r = range reading
                ((sx - r, sy - r), (sx + r, sy + r))

range ((sx,  sy), (bx,  by)) = abs (bx - sx) + abs (by - sy)

readReport :: [String] -> Report
readReport lines = do
    map readLine lines 
    where
        readLine line = do
            let parts = Split.splitOn " " line
            let [sx, sy, bx, by] = [2, 3, 8, 9] & map (readPart . (parts !!))
            ((sx,  sy), ( bx,  by))
        readPart part = 
            part & (dropWhileEnd (not . Char.isNumber) . drop 2) & read :: Int
