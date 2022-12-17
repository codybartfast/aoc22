{-# LANGUAGE TupleSections #-}
module Day16 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.List.Split as Split
import qualified Data.Map as Map

data Valve = Valve 
    { name :: String
    , rate :: Int
    , leads :: [String]
    } deriving (Show)

data Helper = Helper 
    { route :: [String]
    , timeLeft :: Int
    , extraRate :: Int
    } deriving (Show, Eq)

data State = State 
    { helpers :: [Helper]
    , minsLeft :: Int
    , targetsLeft :: [String]
    , aggRate :: Int
    , reduction :: Int
    } deriving (Show, Eq)

solve input lines = do
    let valveList = readValves lines
    let getValve = getValveFromList valveList
    let getDist = getDistanceFromValves valveList getValve
    let allTargets = valveList & filter ((> 0) . rate) & map name
    let add = addRoute getValve getDist
    let expand = includeAndExpand add
    let expandAndPrune = prune 5000 . expand
    let start = getStart allTargets 26

    print $
        iterate expandAndPrune [start] !! length allTargets  
        & head & waitOut

prune size states =
    states
    & filter ((> 0) . timeLeft . fstHlp)
    & nub
    & sortOnDesc waitOut
    & take size

includeAndExpand add states = states ++ expandStates add states

expandStates add states = do concatMap (expandState add) states

expandState add state = map (add state) (targetsLeft state)

addRoute getValve getDist state next = do
    let nextValve = getValve next
    let (first:rest) = helpers state
    let route' = next : route first
    let travelTime = getDist (head (route first)) next + 1
    let timeLeft' = timeLeft first - travelTime
    let minsLeft' = timeLeft first
    let extraRate' = rate nextValve
    let targetsLeft' = delete next (targetsLeft state)
    let reduction' =
            reduction state + ((minsLeft state - minsLeft') * aggRate state)
    let aggRate' = aggRate state + extraRate first
    let first' = Helper 
            {route = route'
            , timeLeft = timeLeft'
            , extraRate = extraRate'
            }
    State { helpers = sortOnDesc timeLeft (first' : rest)
          , minsLeft = minsLeft'
          , targetsLeft = targetsLeft'
          , aggRate = aggRate'
          , reduction = reduction' }

fstHlp state = helpers state & head

waitOut state = do
    let (first:second:_) = helpers state
    let preSpan = minsLeft state - maxZero (timeLeft first)
    let reduction' = reduction state + preSpan * aggRate state
    let aggRate' = aggRate state + extraRate first

    let firstSpan = maxZero (timeLeft first) - maxZero (timeLeft second)
    let reduction'' = reduction' + firstSpan * aggRate'
    let aggRate'' = aggRate' + extraRate second

    let secondSpan = maxZero (timeLeft second)
    let reduction''' = reduction'' + secondSpan * aggRate''

    reduction'''

getStart targets time =
    State { helpers = 
                [ Helper {route = ["AA"], timeLeft = time, extraRate = 0 }
                , Helper {route = ["AA"], timeLeft = time, extraRate = 0 } ]
          , minsLeft = time
          , targetsLeft = targets
          , aggRate = 0
          , reduction = 0 }
          
getDistanceFromValves valves getValve = do
    \ n1 n2 -> do
        let (Just distance) = Map.lookup (n1, n2) distMap
        distance
    where
        addValve distMap v = do
            let pairs = map (:[name v]) (leads v)
            foldl (\ dm [n1, n2] -> Map.insert (n1, n2) 1 dm) distMap pairs

        neighbourDist = foldl addValve Map.empty valves

        addIfShorter dist dm valvePair = do
            let existing = Map.lookup valvePair dm
            case existing of
                    Nothing -> Map.insert valvePair dist dm
                    (Just existingDist) ->
                        if dist < existingDist then
                            Map.insert valvePair dist dm
                        else
                            dm

        expand dm oDist =
                dm & Map.toList & filter ((== oDist) . snd)
                    & concatMap (\ ((n1, n2), d) -> do
                        leads (getValve n2) & filter (/= n1) & map (n1,) )
                        & nub
                        & sort
                        & foldl (addIfShorter (oDist + 1)) dm 
        
        distMap = (neighbourDist, 1) & 
                unfoldr (\ (dm, d) -> do
                    let expanded = expand dm d
                    let r = (expanded, d + 1)
                    Just (r, r) )
                & pairwise
                & dropWhile (\ (a, b) -> fst a /= fst b)
                & head & fst & fst

getValveFromList valveList = do
    let vMap = valveList & map (\ v -> (name v, v)) & Map.fromList
    \ name -> Map.lookup name vMap & \ (Just vlv) -> vlv

readValves lines = do
    map readValve lines 
    where
        readValve line = do
            let parts = Split.splitOn " " line
            let id = parts !! 1
            let rate = 
                    parts !! 4 & Split.splitOn "=" & (!! 1) & init & read :: Int
            let reachable = drop 9 parts & map (dropWhileEnd (== ','))
            Valve {name = id, rate = rate, leads = reachable}

pairwise l = zip l (tail l)

sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse . sort

sortOnDesc pred = reverse . sortOn pred

maxZero = max 0
