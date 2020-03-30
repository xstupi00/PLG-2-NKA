{-|
Module      : ParseInput
Description : Module for the processing of the input file
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module contains the functions to load and validate
the input file with the right linear grammar in the
specified format.
-}
module ParseInput where

import ErrorControl
import DataStructures
import GrammarControl
import Helpers
import ParseArgs

import Control.Monad
import Data.Char
import Data.List
import System.Exit

parseInput :: String -> IO Grammar
parseInput file = do
  grammar <-
    if file == "-"
      then getContents
      else readFile file
  when (null (lines grammar)) $ exitWithErrMsg (ExitFailure 1) missingContent
  variables <-
    validateSymbols isAsciiUpper $ filter (/= "") $ map strip $ splitBy ',' $ head $ lines grammar
  when ((==) 1 (length (lines grammar))) $ exitWithErrMsg (ExitFailure 1) (missingSymbols isAsciiLower)
  terminals <-
    validateSymbols isAsciiLower $ filter (/= "") $ map strip $ splitBy ',' $ (!! 1) $ lines grammar
  when ((==) 2 (length (lines grammar))) $ exitWithErrMsg (ExitFailure 1) (invalidStartSymbol 2)
  startSymbol <- validateSymbols isAsciiUpper [strip ((!! 2) $ lines grammar)]
  productions <- validateProductions $ filter (/= "") $ map strip $ drop 3 $ lines grammar
  return
    Grammar
      { variables = variables
      , terminals = terminals
      , startSymbol = head startSymbol
      , productions = removeEpsilon productions
      }

validateSymbols :: (Char -> Bool) -> [String] -> IO [String]
validateSymbols symbolGroup symbols = do
  when
    (symbolTuples /= [])
    (printWarning $ symbolErrMsg False symbolTuples wrongSymbols symbolGroup)
  when
    (containsInvalidSymbol symbolGroup symbols || isNotGrammarSymbol symbols)
    (exitWithErrMsg (ExitFailure 1) $ symbolErrMsg True symbolTuples wrongSymbols symbolGroup)
  when (null $ nub symbols) $ exitWithErrMsg (ExitFailure 1) (missingSymbols symbolGroup)
  return $ nub symbols
  where
    symbolTuples = filter ((> 1) . snd) . map (\l@(x:xs) -> (x, length l)) . group . sort $ symbols
    wrongSymbols =
      (symbols \\ filter (all symbolGroup) symbols) `union` filter ((> 1) . length) symbols

validateProductions :: [String] -> IO [(String, String)]
validateProductions productions = do
  when (errArrowIndices /= []) $
    exitWithErrMsg
      (ExitFailure 1)
      (productionErrMsg invalidLeftSides (getInvalidRightSides rightSide) 0)
  let rightSide' = map (strip . snd . splitAt 2) rightSide
  when
    (containsInvalidSymbol isAsciiUpper leftSide) -- || isNotGrammarSymbol rightSide
    (exitWithErrMsg
       (ExitFailure 1)
       (productionErrMsg invalidLeftSides (getInvalidLeftSides leftSide) 1))
  when
    (containsInvalidSymbols rightSide')
    (exitWithErrMsg
       (ExitFailure 1)
       (productionErrMsg invalidLeftSides (getInvalidRightSides rightSide') 2))
  when (null $ zip leftSide rightSide') $ exitWithErrMsg (ExitFailure 1) (missingSymbols isSpace)
  return $ zip leftSide rightSide'
  where
    splittedProductions = map (span (/= '-') . strip) productions
    leftSide = map (strip . fst) splittedProductions
    rightSide = map (strip . snd) splittedProductions
    arrowIndices = map (findString "->") rightSide
    errArrowIndices = [0 .. genericLength productions - 1] \\ elemIndices 0 arrowIndices
    invalidLeftSides = zip errArrowIndices (filter (/= 0) arrowIndices)