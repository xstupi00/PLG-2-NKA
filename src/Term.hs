module Term where

import System.IO

work :: [String] -> [String]
work [] = []
work (('+':'+':line):lines) = line : work lines
work (line:lines) = line : work lines

fdup :: String -> IO()
fdup fileName = do
  handle <- openFile fileName ReadMode
  content <- hGetContents handle
  putStr $ unlines $ work $ lines content


getMaxLine :: [String] -> Int
getMaxLine [] = 0
getMaxLine [l] = length l
getMaxLine (l:ls) = if length l > maxLine then length l else maxLine
  where maxLine = getMaxLine ls

getMinLine :: [String] -> Int
getMinLine [] = 0
getMinLine [l] = length l
getMinLine (l:ls) = if length l < minLine then length l else minLine
  where minLine = getMinLine ls
  
showLines :: [String] -> Int -> Int -> [String]
showLines [] _ _ = []
showLines (line:lines) max min = 
  (show showMin ++ "#" ++ show showMax ++ ": " ++ line) : showLines lines max min
    where 
      len = length line
      showMax = if len - max > 0 then len - max else max - len
      showMin = if len - min > 0 then len - min else min - len

prAno :: String -> IO()
prAno fileName = do
  handle <- openFile fileName ReadMode
  content <- hGetContents handle
  let maxLine = getMaxLine $ lines content
  let minLine = getMinLine $ lines content
  putStr $ unlines $ showLines (lines content) maxLine minLine