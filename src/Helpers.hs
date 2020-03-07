module Helpers where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Foldable (Foldable)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

containsInvalidSymbol :: (Char -> Bool) -> [String] -> Bool
containsInvalidSymbol group symbols =
  (||) (not (all (all group) symbols)) (any (all isSpace) symbols)

isNotGrammarSymbol :: [String] -> Bool
isNotGrammarSymbol = any ((> 1) . length)

splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy ch = filter (notElem ch) . groupBy ((==) `on` (== ch))

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

getInvalidLeftSides :: (Eq (t Char), Foldable t) => [t Char] -> [(Int, t Char)]
getInvalidLeftSides vars = zip (findIndices (`elem` invalidLeftSides) vars) invalidLeftSides
  where
    invalidLeftSides = vars \\ filter (all isAsciiUpper) vars `union` filter (all isSpace) vars

getValidatedRightSides = map (all isValidRightSide)
  where
    isAsciiAlpha ch = (||) (isAsciiUpper ch) (isAsciiLower ch)
    isValidRightSide ch = (||) (isAsciiAlpha ch) (ch == '#')

containsInvalidSymbols :: [String] -> Bool
containsInvalidSymbols symbols =
  (||) (not $ all (== True) $ getValidatedRightSides symbols) ("" `elem` symbols)

getInvalidRightSides symbols =
  map
    (\idx -> (idx, (!!) symbols idx))
    (elemIndices False (getValidatedRightSides symbols) `union` elemIndices "" symbols)