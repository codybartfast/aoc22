module Day07 (solve) where

import Data.Function
import Data.List
import qualified Data.Map as Map

-- data Dir = Node { path :: [String]
--                  , subDirs :: [Dir]
--                  , files :: (Int, String)
--                  }

solve input rawLines = do
    let lines = map words rawLines
    let crs = commandResponses lines
    print (pathListing crs)
    putStrLn ""

pathListing commandResponses = do
    pls & reverse & Map.fromList
    where
        pls = parse [] [] commandResponses

        parse pls path [] =
            pls
        parse pls path ((["cd", dir], _):crs') =
            parse pls (updatePath path dir) crs'
        parse pls path ((["ls"], listing):crs') =
            parse ((path, readListing path listing):pls) path crs'

        updatePath path dir = case dir of "/" -> ["/"]
                                          ".." -> tail path
                                          _ -> dir:path

        readListing path listing = do
            let dirs = 
                    filter ((== "dir") . head) listing
                    & map (\[_, name]  -> reverse (name:path))
            let files = 
                    filter ((/= "dir") . head) listing
                    & map (\[size, name]  -> (read size :: Int, name))
            (dirs, files)

            
commandResponses lines = do
    crs
    where
        parse crs part [] = part:crs & reverse & tail
        parse crs part (("$":restWords):restLines) =
                parse (part:crs) (restWords,[]) restLines
        parse crs (command, responses) (response:restLines) =
                parse crs (command, response:responses) restLines
        crs = parse [] ([],[]) lines
