module Helpers where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

containsInvalidSymbol :: (Char -> Bool) -> [String] -> Bool
containsInvalidSymbol group symbols = not (all (all group) symbols)

isNotGrammarSymbol :: [String] -> Bool
isNotGrammarSymbol = any ((> 1) . length)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy ch = filter (notElem ch) . groupBy ((==) `on` (== ch))

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

getInvalidLeftSides :: (Eq (t Char), Foldable t) => [t Char] -> [(Int, t Char)]
getInvalidLeftSides vars = zip (findIndices (`elem` invalidLeftSides) vars) invalidLeftSides
  where
    invalidLeftSides = vars \\ filter (all isAsciiUpper) vars

getInvalidArrows :: [String] -> [(Int, Int)]
getInvalidArrows productions =
  zip (getInvalidIndices productions) (filter (/= 1) (map (findString "->") productions))

getInvalidIndices productions =
  [0 .. genericLength productions - 1] \\ elemIndices 1 (map (findString "->") productions)

getValidatedRightSides symbols =
  zipWith (||) (map (all isAsciiAlpha) symbols) (map (== "#") symbols)
  where
    isAsciiAlpha ch = (||) (isAsciiUpper ch) (isAsciiLower ch)

containsInvalidSymbols :: [String] -> Bool
containsInvalidSymbols symbols = not $ all (== True) $ getValidatedRightSides symbols

getInvalidRightSides symbols =
  map (\idx -> (idx, (!!) symbols idx)) (elemIndices False (getValidatedRightSides symbols))