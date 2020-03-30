{-|
Module      : GrammarControl
Description : Controller for the processing of grammar.
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module contains the functions to validate the grammar,
functions to processing of grammar productions, respectively
to filter the productions of the specific type.
-}
module GrammarControl where

import DataStructures
import ErrorControl
import Helpers

import Control.Monad
import Data.Char
import Data.List
import System.Exit

removeEpsilon :: [(a, String)] -> [(a, String)]
removeEpsilon = map removeEpsilon'
  where
    removeEpsilon' production =
      if any isAsciiAlpha rightSide
        then (leftSide, remove "#" rightSide)
        else (leftSide, nub rightSide)
      where
        leftSide = fst production
        rightSide = snd production

validateGrammar :: Grammar -> IO Grammar
validateGrammar grammar = do
  when (missingStartSymbol grammar) $ exitWithErrMsg (ExitFailure 1) (invalidStartSymbol 0)
  when (missingRuleWithStartSymbol grammar) $ exitWithErrMsg (ExitFailure 1) (invalidStartSymbol 1)
  when (invalidProductions /= []) $
    exitWithErrMsg (ExitFailure 1) (invalidProductionsErrMsg invalidProductionsTuple)
  return grammar
  where
    invalidProductions = filterProductions grammar
    invalidProductionsCodes = map (analyseInvalidProductions grammar) invalidProductions
    invalidProductionsTuple =
      zip3
        (findIndices (`elem` invalidProductions) (productions grammar))
        invalidProductions
        invalidProductionsCodes

missingStartSymbol :: Grammar -> Bool
missingStartSymbol grammar = startSymbol grammar `notElem` variables grammar

missingRuleWithStartSymbol :: Grammar -> Bool
missingRuleWithStartSymbol grammar =
  not (any (\(x, _) -> x == startSymbol grammar) (productions grammar))

filterProductions :: Grammar -> [(String, String)]
filterProductions grammar =
  products \\
  (epsilonProductions `union` basicProductions `union` terminalProductions `union` rightProductions `union`
   simpleProductions)
  where
    epsilonProductions = filterEpsilonProductions products vars terms
    basicProductions = filterBasicProductions products vars terms
    terminalProductions = filterTerminalProductions products vars terms
    rightProductions = filterRightProductions products vars terms
    simpleProductions = filterSimpleProductions products vars terms
    products = productions grammar
    vars = variables grammar
    terms = terminals grammar

filterSimpleProductions :: Eq t1 => [([t1], [t1])] -> [[t1]] -> t -> [([t1], [t1])]
filterSimpleProductions products vars terms =
  filter (\(l, r) -> l `elem` vars && length r == 1 && [head r] `elem` vars) products

filterEpsilonProductions :: Eq a => [(a, String)] -> [a] -> t -> [(a, String)]
filterEpsilonProductions products vars terms =
  filter (\(l, r) -> l `elem` vars && length r == 1 && head r == '#') products

filterBasicProductions :: Eq t => [([t], [t])] -> [[t]] -> [[t]] -> [([t], [t])]
filterBasicProductions products vars terms =
  filter
    (\(l, r) -> l `elem` vars && length r == 2 && [head r] `elem` terms && [last r] `elem` vars)
    products

filterTerminalProductions :: (Eq a, Eq t) => [(a, [t])] -> [a] -> [[t]] -> [(a, [t])]
filterTerminalProductions products vars terms =
  filter (\(l, r) -> l `elem` vars && all (\ch -> [ch] `elem` terms) r) products

filterRightProductions :: Eq t => [([t], [t])] -> [[t]] -> [[t]] -> [([t], [t])]
filterRightProductions products vars terms =
  filter
    (\(l, r) ->
       l `elem` vars &&
       length r > 2 &&
       all (\ch -> [ch] `elem` terms) (fst $ splitAt (length r - 1) r) && [last r] `elem` vars)
    products

analyseInvalidProductions :: Grammar -> (String, String) -> (Int, String)
analyseInvalidProductions grammar (left, right)
  | left `notElem` vars = (0, left)
  | any (\ch -> [ch] `notElem` vars) (filter isAsciiUpper right) =
    (1, filter (\ch -> [ch] `notElem` vars) (filter isAsciiUpper right))
  | any (\ch -> [ch] `notElem` terms) (filter isAsciiLower right) =
    (2, filter (\ch -> [ch] `notElem` terms) (filter isAsciiLower right))
  | otherwise = (-1, "")
  where
    vars = variables grammar
    terms = terminals grammar