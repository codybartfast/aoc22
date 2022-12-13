module Day13 (solve) where

import Data.Function ( (&) )
import Data.List

data Msg = Num Int | List [Msg] deriving (Show, Eq)

instance Ord Msg where
    compare (Num a) (Num b) = compare a b
    compare (List a) (List b) = compare a b
    compare number@(Num _) list@(List _) = compare (List [number]) list
    compare list@(List _) number@(Num _) = compare list (List [number]) 

solve input lines = do
    let pairs = paras lines
    print $
        pairs
        & map (\ [l1, l2] -> readMsg l1 < readMsg l2)
        & zip [1..]
        & filter snd
        & map fst
        & sum

    let (div2, div6) = (readMsg "[[2]]", readMsg "[[6]]")
    let sorted = 
            lines
            & filter (/= "")
            & map readMsg
            & (\ lns -> div2:div6:lns)
            & sort
    let (Just idx1, Just idx2) = (elemIndex div2 sorted, elemIndex div6 sorted)
    print $ (idx1 + 1) * (idx2 + 1)

readMsg line = do
    readMsg line & fst
    where
        readList :: [Msg] -> String -> (Msg, String)
        readList parts (']':rest) = 
            (List (reverse parts), rest)
        readList parts (',':rest) = 
            readList parts rest
        readList parts txt = do
            let (msg, rest') = readMsg txt
            readList (msg:parts) rest'

        readNumber :: [Char] -> String -> (Msg, String)
        readNumber digits (d:rest@(nxt:_))
            | nxt == ',' || nxt == ']' = (Num (read (reverse (d:digits))), rest)
            | otherwise = readNumber (d:digits) rest

        readMsg :: String -> (Msg, String)
        readMsg ('[':rest) = readList [] rest
        readMsg txt = readNumber [] txt

paras = 
    paras [[]]
    where 
        paras :: [[String]] -> [String] -> [[String]]
        paras grps [] = grps & map reverse & reverse
        paras grps ("":lns) = paras ([]:grps) lns
        paras (grp:grps) (ln:lns) = paras ((ln:grp):grps) lns        
