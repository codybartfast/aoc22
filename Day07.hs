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
    let sizes = allDirs tree & map size & sort
    print $ sizes & filter (< 100000)  & sum

    let minDel = size tree - (70000000 - 30000000)
    print $ sizes & filter (>= minDel) & head

allDirs dir = dir : concatMap allDirs (subDirs dir)

-- tree representing the know file system
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

-- map from a path to a pair comprising its child paths and files for that path
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
            let subPaths = 
                    filter ((== "dir") . head) listing
                    & map (\[_, name]  -> reverse (name:path))
            let files = 
                    filter ((/= "dir") . head) listing
                    & map (\[size, name]  -> (read size :: Integer, name))
            (subPaths, files)

-- pairs comprsing a command and the response to that command
commandResponses lines = do
    crs
    where
        parse crs partialResponse [] = partialResponse:crs & reverse & tail
        parse crs partialResponse (("$":restWords):restLines) =
                parse (partialResponse:crs) (restWords,[]) restLines
        parse crs (command, responses) (response:restLines) =
                parse crs (command, response:responses) restLines
        crs = parse [] ([], []) lines
