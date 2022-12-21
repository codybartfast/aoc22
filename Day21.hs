-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE LambdaCase #-}
module Day21 (solve) where


import Data.Function ( (&) )
import Data.List
-- import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Map as Map
-- import qualified Data.Set as Set

data Job = Value Int | Op (String, String, String, Int -> Int -> Int)

instance Show Job 
    where 
        show (Value i) = show i
        show (Op (m1, m2, optText, _)) = show (m1, m2, optText)

solve input lines = do
    let jobs = readJobs lines
    print $ yell "root" jobs Map.empty

yell monkey jobs known = do
    let (Just job) = Map.lookup monkey jobs
    eval job (Map.lookup monkey known)
    where
        eval job (Just v) = (v, known)
        eval (Value v) Nothing = (v, Map.insert monkey v known)
        eval (Op (m1, m2, _, op)) Nothing= do
            let (v1, known') = yell m1 jobs known
            let (v2, known'') = yell m2 jobs known
            let v = op v1 v2
            (v, Map.insert monkey v known'')


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
