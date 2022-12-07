module Day07 (solve) where

import Data.Function
import Data.List
import qualified Data.Map as Map

data Dir = Dir { path :: [String]
                 , subDirs :: [Dir]
                 , files :: [(Integer, String)]
                 , size :: Integer
                 } deriving (Show)

solve input rawLines = do
    let lines = map words rawLines
    let crs = commandResponses lines
    let pls = pathListings crs
    let tree = buildTree pls ["/"]
    let sizes = allNodes tree & map size & sort
    print $ sizes & filter (< 100000)  & sum

    let minDel = size tree - (70000000 - 30000000)
    print $ sizes & filter (>= minDel) & head

allNodes node = node : concatMap allNodes (subDirs node)

buildTree pathListings path = do
    let Just (subPaths, files) = Map.lookup path pathListings
    let subDirs = map (buildTree pathListings) subPaths
    let sizeFiles = files & map fst & sum
    let sizeDirs = subDirs & map size & sum
    Dir { path = path
         , subDirs = subDirs
         , files = files 
         , size = sizeFiles + sizeDirs
         }

pathListings commandResponses = do
    pls & reverse & Map.fromList
    where
        pls = parse [] [] commandResponses

        parse pls path [] =
            pls
        parse pls path ((["cd", dir], _):crs') =
            parse pls (updatePath path dir) crs'
        parse pls path ((["ls"], listing):crs') =
            parse ((reverse path, readListing path listing):pls) path crs'

        updatePath path dir = case dir of "/" -> ["/"]
                                          ".." -> tail path
                                          _ -> dir:path

        readListing path listing = do
            let dirs = 
                    filter ((== "dir") . head) listing
                    & map (\[_, name]  -> reverse (name:path))
            let files = 
                    filter ((/= "dir") . head) listing
                    & map (\[size, name]  -> (read size :: Integer, name))
            (dirs, files)

            
commandResponses lines = do
    crs
    where
        parse crs part [] = part:crs & reverse & tail
        parse crs part (("$":restWords):restLines) =
                parse (part:crs) (restWords,[]) restLines
        parse crs (command, responses) (response:restLines) =
                parse crs (command, response:responses) restLines
        crs = parse [] ([], []) lines
