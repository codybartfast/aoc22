{-# LANGUAGE TupleSections #-}
module Day22 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Set as Set

-- It ain't DRY, it's turkey driven code

solve input lines = do
    part1 lines

    let [grove, [instrTxt]] = lines & Split.splitOn [""]
    let start = getStart grove
    let instrs = readInstrsWithF instrTxt
    let dim = getDim grove
    let cube = getCube dim grove start lines
    let (surfCoord, s0 : _) = navigate grove dim cube instrs
    let Just groveCoord = Map.lookup surfCoord s0
    print $ password (groveCoord, face s0)

face surface = do
    let Just (x, y) = Map.lookup (0, 0) surface
    let Just (x', y') = Map.lookup (0, 1) surface
    let delta = (x' - x, y' - y)
    case delta of (1, 0) -> 0; (0, 1) -> 1; (-1, 0) -> 2; (0, -1) -> 3

navigate grove dim cube instrs =
    foldl tryInstrCube cube instrs
    where

        tryInstrCube cube (dir, dist) = do
            let cube' =
                    case dir of
                                -1 -> leftCube dim cube
                                0 -> cube
                                1 -> rightCube dim cube
                                _ -> error "Unexplected"
            let cube'' =
                    forwardN cube' dist
            cube''
        forwardN cube n = do
            let path = iterate forwardOne cube & take (n + 1) & takeWhile isFree
            last path
        forwardOne ((x, y), ss) = do
            let y' = y + 1
            if y' < dim then ((x, y'), ss) else ((x, 0), spin dim ss)
        isFree cube = value cube == '.'
        value (coord@(x, y), s0 : _ : s2 : _) = do
            let (Just (gx, gy)) = Map.lookup coord s0
            grove !! gy !! gx

getCube dim grove start lines = do
    let pathsToSquares = getPathsToSquares dim grove start
    let paths = pathsToSquares & sortOn length
    let incPaths = incrementalPaths paths
    let rpath = reversePath (last paths)
    let cube =
            foldl (\ (pos, cube) path -> do
                let pos' = applyPathToGrove pos path
                let cube' = applyPathToCube dim cube path
                let cube'' = mapSurfaceToSquare grove dim pos' cube'
                (pos', cube''))
            (start, emptyCube) incPaths
            & snd
            & (\ cube -> applyPathToCube dim cube rpath)
    cube

reversePath path = do
    let rpath = (1,0) : (1,0) : reverse path
    let pairs = zip rpath (tail rpath)
    pairs
        & map (\ ((nxtDir, nxtDist), (prevDir, prevDist)) ->
            (flipDir nxtDir, prevDist))
        & (++ [(1, 0), (1, 0)])
    where
        flipDir d = case d of -1 -> 1; 1 -> -1 ; 0 -> 0

emptyCube = ((0, 0), [0 .. 5] & map (const Map.empty))

mapSurfaceToSquare grove dim ((x, y), f) (surfPos, surf : rest) = do
    let diff = case f of
                    0 -> \(x , y) -> (y, x)
                    1 -> \(x , y) -> (-x, y)
                    2 -> \(x , y) -> (-y, -x)
                    3 -> \(x , y) -> (x, -y)
    let coords =
            [0 .. dim - 1] & concatMap (\ y ->
                [0 .. dim - 1] & map (,y))
    let groveCoord surfCoord = do
            let (dx, dy) = diff surfCoord
            (x + dx, y + dy)
    let lookup surfCoord = do
            let (x, y) = groveCoord surfCoord
            grove !! y !! x
    let surface =
            Map.fromList $ coords
            & map (\ coord -> (coord, groveCoord coord))
    (surfPos, surface : rest)

incrementalPaths paths =
    zip paths (tail paths)
    & map (\ (p, p') -> drop (length p) p')
    & (head paths : )

applyPathToCube dim cube path = do
    foldl applyInstrCube cube path
    where
        applyInstrCube cube (dir, dist) = do
            let cube' = case dir of
                            -1 -> leftCube dim cube
                            0 -> cube
                            1 -> rightCube dim cube
            iterate (forwardCube dim) cube' !! dist

getPathsToSquares dim grove pos = do
    let paths = unfoldr nextSquare ([], pos)
    uniqueSquares paths & dropWhile ((< 6) . length)
        & head
        & Map.toList
        & map (fst . snd)
    where
        uniqueSquares sqrPaths = do
            scanl
                (\ mp path@(_, pos) -> do
                    let sqrId = squareId pos
                    if not $ Map.member sqrId mp
                        then  Map.insert sqrId path mp
                        else mp)
                Map.empty sqrPaths
        walkSquares grove pos = unfoldr nextSquare ([], pos)
        nextSquare (legs, pos) = do
            let candidates = map (\ leg -> (legs ++ leg, applyPathToGrove pos leg)) candRoutes
            let next = candidates & filter (notEmpty . snd) & head
            Just ((legs, pos), next)
        notEmpty ((x, y), f) =
            y >= 0 && y < length grove
            && x >= 0 && x < length (grove !! y)
            && grove !! y !! x /= ' '
        candRoutes =
            [ readInstrs $ "L1"
            , readInstrs $ "F" ++ dimStr
            , readInstrs $ "F" ++ dimStrDec ++ "R" ++ dimStr
            , readInstrs $ "R" ++ dimStrDec ++ "R1" ]
        width = grove & map length & maximum
        dimStr = show dim
        dimStrDec = show (dim - 1)
        height = length grove
        sqrsHigh = height `div` dim
        sqrsWide = width `div` dim
        squareId ((x, y), _) =  x `div` dim + sqrsWide * (y `div` dim)

applyPathToGrove pos instrs = do
    foldl applyStep pos instrs
    where
        applyStep (coord, f) (turn, dist) = do
            let f' = (f + turn)  `mod` 4
            let mv = case f' of
                    0 -> \ (x, y) -> (x + 1, y)
                    1 -> \ (x, y) -> (x, y + 1)
                    2 -> \ (x, y) -> (x - 1, y)
                    3 -> \ (x, y) -> (x, y - 1)
            let coord' = iterate mv coord !! dist
            (coord', f')

rightCube dim = leftCube dim . leftCube dim . leftCube dim
leftCube dim ((x,y), surfaces) = do
    let max = dim - 1
    ((y, max - x), clockwise dim surfaces)
forwardCube dim ((x,y), surfaces) = do
    let y' = y + 1
    if y' < dim then ((x, y'), surfaces) else ((x, 0), spin dim surfaces)
clockwise dim [s0, s1, s2, s3, s4, s5] =
            [surfaceClockwise dim s0, s4, surfaceAnticlock dim s2,
                surfaceOneEighty dim s5, surfaceOneEighty dim s3, s1]
--               clockwise
--             3          u5
--             2          a2
--             1           4
--          4  0  5    u3 c0 1

spin dim [s0, s1, s2, s3, s4, s5] =
        [s1, s2, s3, s0, surfaceClockwise dim s4, surfaceAnticlock dim s5]
--                  spin
--             3            0
--             2            3
--             1            2
--          4  0  5     c4  1  a5

surfaceAnticlock dim = surfaceClockwise dim . surfaceOneEighty dim
surfaceOneEighty dim = surfaceClockwise dim . surfaceClockwise dim
surfaceClockwise dim = Map.fromList . flipVert dim . transposeKvps . Map.toList
transposeKvps kvps = kvps & map (\ ((x, y), v) -> ((y, x), v))
flipVert dim kvps = do
        let max = dim - 1
        kvps & map (\ ((x, y), v) -> ((x, max - y), v))


getDim grove =
    grove & concatMap (filter (/= ' ')) & length & (`div` 6)
    & fromIntegral & sqrt & floor


----------------------------------  PART ONE  ----------------------------------

part1 lines = do
    let (grove, instrs) = readGrove1 lines
    let start = getStart lines
    let segments = foldl (addSegment grove) [[start]] instrs
    let final = segments & head & last
    print $ final & password

password ((x, y), f) = 1000 * (y + 1) + 4 * (x + 1) + f

getStart lines = (( head lines & filter (== ' ') & length, 0), 0)

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
            if isWall pos then Nothing else Just (pos, nextPos pos)
        isWall (coord, dir) = Set.member coord (walls grove)
        nextPos ((x, y), dir) =
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

readGrove1 lines = do
    let route = lines !! (gap + 1)
    let rows = map tiles groveTxt
    let cols = map tiles (transpose groveTxt)
    let grove = Grove
            { rows = rows
            , cols = cols
            , walls = walls }
    let instrs = readInstrsWithF (lines !! (gap + 1))
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

readInstrsWithF txt = readInstrs ('F' : txt)

readInstrs =
    readInstructins
    where
        readInstructins [] = []
        readInstructins routeTxt = do
            let (chunk, rest) = readSingle routeTxt
            chunk : readInstructins rest
        readSingle instrTxt = do
            let t = head instrTxt
            let ds = takeWhile Char.isNumber (tail instrTxt)
            let chunk = t : ds
            let turn = case t of 'L' -> -1; 'R' -> 1; 'F' -> 0
            ((turn, read ds :: Int), drop (length chunk) instrTxt)
