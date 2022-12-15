module Day15 (solve) where

import Data.Function ( (&) )
import Data.List 
import qualified Data.Char as Char
import qualified Data.List.Split as Split

type Report = [((Int, Int), (Int, Int))]

solve input lines = do
    let report = readReport lines
    let row = if length report == 14 then 10 else 2000000
    let darkDiamonds = [((row, row), row * 2)]
    let scanDiamonds = report & map (\ r@(s, b) -> (s, range r))

    let darkSqs = map toSquare darkDiamonds
    let scanSqs = map toSquare scanDiamonds

    print $ report & cannotContain row
    print $ foldl removeMany darkSqs scanSqs
        & filter (\ (a, b) -> a == b)
        & head & fst & fromSquarePoint
        & (\ (a, b) -> a * 4000000 + b)

removeMany darkSqs scan = concatMap (remove scan) darkSqs

remove scan dark  = do
    filter isReal [topRect, leftRect, rightRect, bottomRect]
    where
        left rect = rect & fst & fst
        bottom rect = rect & fst & snd
        right rect = rect & snd & fst
        top rect = rect & snd & snd

        topRect = 
            ((left dark, 
                max (bottom dark) (top scan + 1)),
             (right dark, 
                top dark))
        leftRect =
            ((left dark, 
                bottom dark),
             (min (right dark) (left scan - 1), 
                min (top dark) (bottom topRect - 1)))
        rightRect =
            ((max (left dark) (right scan + 1), 
                bottom dark),
             (right dark, 
                top leftRect))
        bottomRect =
            ((max (left dark) (left scan), 
                bottom dark),
             (min (right scan) (right dark), 
                min (top dark) (bottom scan - 1)))
            
        isReal rectangle =
            (top rectangle >= bottom rectangle)
             && (left rectangle <= right rectangle)
        
toSquare ((x, y), r) = do
    let x' = x - y
    let y' = x + y
    ((x' - r, y' - r), (x' + r, y' + r))

fromSquarePoint (x, y) = ((x + y) `div` 2, (y - x) `div` 2)

cannotContain row report = do
    let (minX, maxX) = reportXRange report
    let beacons = report & map snd
    let beaconsInRow = beacons & filter ((== row) . snd) & nub
    let scanned = [minX .. maxX] & filter inRangeAny
    length scanned - length beaconsInRow
    where
        inRangeAny x = report & any (inRange (x, row))
        inRange pos reading = dist pos (fst reading) <= range reading
        dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

reportXRange report = do
    report
        & map sensorXRange
        & \ ranges -> (minimum $ map fst ranges, maximum $ map snd ranges)
    where
        sensorXRange reading@((sx,  sy), (bx,  by)) = do
            let r = range reading
            (sx - r , sx + r)

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
