{-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day16 (solve) where

import Data.Function ( (&) )
import Data.List
-- import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Map as Map

data Valve = Valve {
    name :: String
    , rate :: Int
    , leads :: [String]
    } deriving (Show)

data State = State 
    { route :: [String]
    , timeLeft :: Int
    , targetsLeft :: [String]
    , aggRate :: Int
    , reduction :: Int
    } deriving (Show)

solve input lines = do
    let valveList = readValves lines
    let getValve = getValveFromList valveList
    let getDist = getDistanceFromValves valveList getValve
    let allTargets = valveList & filter ((> 0) . rate) & map name
    let add = addRoute getValve getDist
    
    print $
        foldl add (start allTargets) ["DD", "BB", "JJ", "HH", "EE", "CC"]
        & waitOut


waitOut state = do
    let reduction' = reduction state + timeLeft state * aggRate state
    state {timeLeft = 0, reduction = reduction'}

start targets =
    State { route = ["AA"]
          , timeLeft = 30
          , targetsLeft = targets
          , aggRate = 0
          , reduction = 0 }

addRoute :: (String -> Valve) -> (String -> String -> Int) -> State -> String -> State
addRoute getValve getDist state next = do
    let nextValve = getValve next
    let route' = next : route state
    let timePassed = getDist (head (route state)) next  + 1
    let timeLeft' = timeLeft state - timePassed
    let targetsLeft' = delete next (targetsLeft state)
    let aggRate' = aggRate state + rate nextValve
    let reduction' = reduction state + (timePassed * aggRate state)
    State { route = route'
          , timeLeft = timeLeft'
          , targetsLeft = targetsLeft'
          , aggRate = aggRate'
          , reduction = reduction' }

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
    \ name ->  Map.lookup name vMap & \ (Just vlv) -> vlv

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