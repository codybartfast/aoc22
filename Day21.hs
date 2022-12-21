module Day21 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.List.Split as Split
import qualified Data.Map as Map

data Job = Value Int | Op (String, String, String, Int -> Int -> Int)

instance Show Job 
    where show (Value i) = show i
          show (Op (m1, m2, optText, _)) = show (m1, m2, optText)

solve input lines = do
    let jobs = readJobs lines
    print $ yell "root" jobs 
    print $ seekEquality jobs

seekEquality jobs = do
    let aSolution = do
            unfoldr (tryFrom jobs) (0, 0)
            & dropWhile ((/= 0) . snd)
            & head
            & fst
    [aSolution, (aSolution - 1)  .. ]
        & takeWhile (\ n -> rootDiff jobs n == 0)
        & last
    where
        mod = if rootDiff jobs 0 < 0 then id else (0-)
        tryFrom jobs (start, _) = do
            [0 .. ]
                & map (\ n -> (n, rootDiff jobs (start + 2^n) & mod))
                & takeWhile ((<= 0) .  snd)
                & last
                & \ (n, diff) -> (start + 2^n, diff)
                & \ r -> Just (r, r)

rootDiff jobs humn = yell "root" (part2Jobs jobs humn)

yell monkey jobs = do
    case Map.lookup monkey jobs of (Just job) -> eval job
    where
        eval (Value v) = v
        eval (Op (m1, m2, _, op)) = do
            let v1 = yell m1 jobs 
            let v2 = yell m2 jobs 
            op v1 v2
            
part2Jobs jobs humn =
    jobs & Map.toList & map substitute & Map.fromList
    where
        substitute ("root", Op (m1, m2, "+", op)) = 
            ("root", Op (m1, m2, "-", (-)))
        substitute ("humn", Value _) = ("humn", Value humn)
        substitute a = a

readJobs lines = do
    map readLine lines & Map.fromList
    where
    readLine line = do
        let parts = Split.splitOn " " line
        let name = head parts & init
        let job = 
                if length parts == 2 then 
                    Value (read (parts !! 1)) 
                else do
                    let m1 = parts !! 1
                    let m2 = parts !! 3
                    let opText = parts !! 2
                    let op = case opText of 
                            "+" -> (+); "-" -> (-); "*" -> (*); "/" -> div
                    Op (m1, m2, opText, op)
        (name, job)
