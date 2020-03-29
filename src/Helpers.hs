{-|
Module      : Helpers
Description : Helpers function for general purpose
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module contains helper functions for general purpose
and their use within different modules.
-}
module Helpers where

import Data.Char
import Data.Foldable (Foldable)
import Data.Function
import Data.List
import Data.Maybe

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
    
getInvalidRightSides :: [String] -> [(Int, String)]
getInvalidRightSides symbols =
  map
    (\idx -> (idx, (!!) symbols idx))
    (elemIndices False (getValidatedRightSides symbols) `union` elemIndices "" symbols)

getValidatedRightSides :: [String] -> [Bool]
getValidatedRightSides = map (all isValidRightSide)
  where
    isValidRightSide ch = (||) (isAsciiAlpha ch) (ch == '#')

isAsciiAlpha :: Char -> Bool
isAsciiAlpha ch = (||) (isAsciiUpper ch) (isAsciiLower ch)

containsInvalidSymbols :: [String] -> Bool
containsInvalidSymbols symbols =
  (||) (not $ all (== True) $ getValidatedRightSides symbols) ("" `elem` symbols)

remove :: String -> String -> String
remove w "" = ""
remove w s@(c:cs)
  | w `isPrefixOf` s = remove w (drop (length w) s)
  | otherwise = c : remove w cs