module Day13 (solve) where

import Control.Monad (forM_)
import Data.Function ( (&) )
import Data.List

data Msg = N Int | L [Msg] deriving (Show, Eq)

instance Ord Msg where
    compare (N a) (N b) = compare a b
    compare (L a) (L b) = compare a b
    compare number@(N _) list@(L _) = compare (L [number]) list
    compare list@(L _) number@(N _) = compare list (L [number]) 

solve input lines = do
    let pairs = paras lines
    print $
        pairs
        & map (\ [l1, l2] -> readMsg l1 < readMsg l2)
        & zip [1..]
        & filter snd
        & map fst
        & sum

readMsg line = do
    readList [] (tail line) & fst
    where
        readList :: [Msg] -> String -> (Msg, String)
        readList parts (']':rest) = 
            (L (reverse parts), rest)
        readList parts (',':rest) = 
            readList parts rest
        readList parts txt = do
            let (msg, rest') = readPart txt
            readList (msg:parts) rest'

        readNumber :: [Char] -> String -> (Msg, String)
        readNumber digits (d:rest@(nxt:_))
            | nxt == ',' || nxt == ']' = (N (read (reverse (d:digits))), rest)
            | otherwise = readNumber (d:digits) rest

        readPart :: String -> (Msg, String)
        readPart ('[':rest) = readList [] rest
        readPart txt = readNumber [] txt

paras = 
    paras [[]]
    where 
        paras :: [[String]] -> [String] -> [[String]]
        paras grps [] = grps & map reverse & reverse
        paras grps ("":lns) = paras ([]:grps) lns
        paras (grp:grps) (ln:lns) = paras ((ln:grp):grps) lns        
