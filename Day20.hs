{-# LANGUAGE TupleSections #-}
module Day20 (solve) where

import Data.Function ( (&) )
import Data.List
import qualified Data.Map as Map

solve input lines = do
    let (nums, message) = readMessage lines 1
    let len = length message
    print $ iterate (mix len nums) message !! 1 & grove len

    let (nums', message') = readMessage lines 811589153
    print $ iterate (mix len nums') message' !! 10 & grove len

grove len msg = do
    let numbers = msg & values & map snd
    numbers !! (1000 `mod` len)
     + numbers !! (2000 `mod` len)
     + numbers !! (3000 `mod` len)

mix len nums message = do foldl (mixOne len) message nums

mixOne len message num@(_, n) = do
    let (_, n) = num
    let n' = n `mod` (len - 1)
    if n' == 0 then message else do
        let target = forward message num (n `mod` (len - 1))
        let msg' = remove message num
        insertAfter msg' target num

insertAfter message num new = do
    let (pPrev, next) = get message num
    let (_, nNext) = get message next
    let msg' = Map.insert num (pPrev, new) message
    let msg'' = Map.insert next (new, nNext) msg'
    let msg''' = Map.insert new (num, next) msg''
    msg'''
    
remove message num = do
    let (prev, next) =  get message num
    let map' = Map.delete num message
    let (pPrev, _) = get map' prev
    let map'' = Map.insert prev (pPrev, next) map'
    let (_, nNext) = get map'' next
    let map''' = Map.insert next (prev, nNext) map''
    map'''

back message num n = iterate (getPrev message) num !! n

getPrev message num = do get message num & fst

forward message num n = iterate (getNext message) num !! n

getNext message num = do get message num & snd

get message num = do
    let (Just entry) = Map.lookup num message
    entry

values message = do
        zero : unfoldr (\ numb -> do
            let nxt@(_, n') = getNext message numb
            if n' == 0 then Nothing else Just (nxt, nxt)) zero
        where
            (Just zero) = message & Map.toList & map fst & find ((==0). snd)

readMessage lines factor = do
    let oNums = map ((* factor) . read) lines :: [Int]
    let uNums = zip [0 .. ] oNums
    let nums = uNums & repeat & concat 
    let trips = 
            zip3 nums (tail nums) (tail (tail nums))
            & take (length lines)
    last trips : init trips
            & map (\ (prev, n, next) -> (n, (prev, next)))
            & Map.fromList
            & (uNums,)
