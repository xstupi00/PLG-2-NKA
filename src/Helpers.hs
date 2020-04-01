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
import Data.Function
import Data.List
import Data.Maybe

-- ^ returns a copy of the string with both leading and trailing characters removed
strip ::
     String -- ^ string for the striping 
  -> String -- ^ reduced copy of the string
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- ^ check whether the given symbols contain some invalid symbols outside the allowed group
containsInvalidSymbol ::
     (Char -> Bool) -- ^ character classification function (isAsciiLower, isAsciiUpper, isSpace)
  -> [String] -- ^ list of symbols to validate 
  -> Bool -- ^ Contain given symbols some invalid symbol?
containsInvalidSymbol group symbols
  -- ^ any symbol outside the given group of some symbol is only whitespace
 = (||) (not (all (all group) symbols)) (any (all isSpace) symbols)

-- ^ check whether the given symbols are grammar symbol, thus have the length equal to one
isNotGrammarSymbol ::
     [String] -- ^ list of symbols to validate 
  -> Bool -- ^ Contain given symbols some no-grammar symbol?
isNotGrammarSymbol = any ((> 1) . length)

-- ^ returns a list of strings after breaking the given string by the specified separator
splitBy ::
     (Eq a)
  => a -- ^ the separator to use when splitting the string
  -> [a] -- ^ string to splitting
  -> [[a]] -- ^ list of string after breaking
splitBy ch = filter (notElem ch) . groupBy ((==) `on` (== ch))

-- ^ returns the location of the specified occurrence of a string within a character expression 
findString ::
     (Eq a)
  => [a] -- ^ string to search for
  -> [a] -- ^ string to search in
  -> Int -- ^ location of the for-string within in-string
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

-- ^ filter invalid left sides of the production with their order numbers
getInvalidLeftSides ::
     [String] -- ^ list of the left sides from all productions
  -> [(Int, String)] -- ^ invalid left sides of the productions and their order numbers
getInvalidLeftSides left_sides =
  zip (findIndices (`elem` invalidLeftSides) left_sides) invalidLeftSides
  -- ^ filter valid left sides, thus all variables or only whitespaces
  where
    invalidLeftSides =
      left_sides \\ filter (all isAsciiUpper) left_sides `union` filter (all isSpace) left_sides

-- ^  filter invalid right sides of the production with their order numbers
getInvalidRightSides ::
     [String] -- ^ list of the right sides from all productions
  -> [(Int, String)] -- ^ invalid left sides of the productions and their order numbers
getInvalidRightSides symbols
  -- ^ filter valid right sides, thus the sequence of symbols of epsilon (#)
 =
  map
    (\idx -> (idx, (!!) symbols idx))
    (elemIndices False (getValidatedRightSides symbols) `union` elemIndices "" symbols)

-- ^ filter only valid right sides of the productions, thus symbols sequence or epsilon (#)
getValidatedRightSides ::
     [String] -- ^ list of right sides from the all productions 
  -> [Bool] -- ^ list of flags, whether the right side is valid or nor
getValidatedRightSides = map (all isValidRightSide)
   -- ^ symbol or epsilon
  where
    isValidRightSide ch = (||) (isAsciiAlpha ch) (ch == '#')

-- ^ isAsciiUpper or isAsciiLower
isAsciiAlpha ::
     Char -- ^ char to validate
  -> Bool -- ^ Is char AsciiUpper or AsciiLower?
isAsciiAlpha ch = (||) (isAsciiUpper ch) (isAsciiLower ch)

-- ^ check whether the given list of symbols contains some invalid symbols
containsInvalidSymbols ::
     [String] -- ^ symbols to validate 
  -> Bool -- ^ Contains the symbols invalid symbol?
containsInvalidSymbols symbols
  -- ^ some invalid or empty symbol
 = (||) (not $ all (== True) $ getValidatedRightSides symbols) ("" `elem` symbols)

-- ^ remove all occurrences of string w from string s1, and store the result in s2.
remove ::
     String -- ^ string w 
  -> String -- ^ string s1
  -> String -- ^ result string s2
remove w "" = ""
remove w s@(c:cs)
  | w `isPrefixOf` s = remove w (drop (length w) s)
  | otherwise = c : remove w cs