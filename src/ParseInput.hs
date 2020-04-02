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

import DataStructures
import ErrorControl
import GrammarControl
import Helpers
import ParseArgs

import Control.Monad
import Data.Char
import Data.List
import System.Exit

-- ^ read and validate the given input with the expected form of grammar
parseInput ::
     String -- ^ input file 
  -> IO Grammar -- ^ constructed grammar data structure or exit program when error occurred
parseInput file
  -- ^ check whether was given the input file or the input will reading from the STDOUT
 = do
  grammar <-
    if file == "-"
      then getContents
      else readFile file
  -- ^ check whether the given input is not empty
  when (null (lines grammar)) $ exitWithErrMsg (ExitFailure 1) missingContent
  -- ^ obtain and validate the variables from the given input (first non-empty line of the input)
  variables <-
    validateSymbols isAsciiUpper $ filter (/= "") $ map strip $ splitBy ',' $ head $ lines grammar
  -- ^ check whether the given input contains the terminals at expected location
  when ((==) 1 (length (lines grammar))) $
    exitWithErrMsg (ExitFailure 1) (missingSymbols isAsciiLower)
  -- ^ obtain and validate the terminals from the given input (second non-empty line of the input)
  terminals <-
    validateSymbols isAsciiLower $ filter (/= "") $ map strip $ splitBy ',' $ (!! 1) $ lines grammar
  -- ^ check whether the given input contains the start symbol at expected location  
  when ((==) 2 (length (lines grammar))) $ exitWithErrMsg (ExitFailure 1) (invalidStartSymbol 2)
  -- ^ obtain and validate the start symbol
  startSymbol <- validateSymbols isAsciiUpper [strip ((!! 2) $ lines grammar)]
  -- ^ the remaining content of the file would be the productions
  productions <- validateProductions $ filter (/= "") $ map strip $ drop 3 $ lines grammar
  -- ^ construct the internal representation of the loaded grammar from given input
  return
    Grammar
      { variables = variables -- ^ V
      , terminals = terminals -- ^ T
      , startSymbol = head startSymbol -- ^ S
      , productions = reduceEpsilon productions -- ^ P (reduce eps: A->a#A => A->aA; A->## => A->#)
      }

-- ^ valid the given symbols whether they have the needed format according to specification
validateSymbols ::
     (Char -> Bool) -- ^ character classification function (isAsciiLower, isAsciiUpper)
  -> [String] -- ^ symbols (variables, terminals, startSymbol)
  -> IO [String] -- ^ return validated symbols or exit program with the error when some error
validateSymbols symbolGroup symbols
  -- ^ check whether all symbols have length equal to one
 = do
  when
    (symbolTuples /= [])
    (printWarning $ symbolErrMsg False symbolTuples wrongSymbols symbolGroup)
  -- ^ check whether all symbols satisfy the requirements
  when
    (containsInvalidSymbol symbolGroup symbols || isNotGrammarSymbol symbols)
    (exitWithErrMsg (ExitFailure 1) $ symbolErrMsg True symbolTuples wrongSymbols symbolGroup)
  --  check whether the given list is not empty when are the validate variables
  when ((&&) (null $ nub symbols) (symbolGroup 'A')) $
    exitWithErrMsg (ExitFailure 1) (missingSymbols symbolGroup)
  -- ^ return the validated symbols
  return $ nub symbols
  where
    symbolTuples = filter ((> 1) . snd) . map (\l@(x:xs) -> (x, length l)) . group . sort $ symbols
    -- ^ construct the error tuples for detail description in the STDERR
    wrongSymbols =
      (symbols \\ filter (all symbolGroup) symbols) `union` filter ((> 1) . length) symbols

-- ^ valid the given set of production whether they are the required format
validateProductions ::
     [String] -- ^ the set of loaded productions from the input 
  -> IO [(String, String)] -- ^ processed production divide to (L, R) from L->R or exit the program
validateProductions productions
  -- ^ check whether the productions contains the arrow on the expected location
 = do
  when (errArrowIndices /= []) $
    exitWithErrMsg
      (ExitFailure 1)
      (productionErrMsg invalidLeftSides (getInvalidRightSides rightSide) 0)
  let rightSide' = map (strip . snd . splitAt 2) rightSide
  -- ^ check whether the left side of production is only the valid variable
  when
    (containsInvalidSymbol isAsciiUpper leftSide) -- || isNotGrammarSymbol rightSide
    (exitWithErrMsg
       (ExitFailure 1)
       (productionErrMsg invalidLeftSides (getInvalidLeftSides leftSide) 1))
  -- ^ check whether the right side of production has the required format (terminals and variables)
  when
    (containsInvalidSymbols rightSide')
    (exitWithErrMsg
       (ExitFailure 1)
       (productionErrMsg invalidLeftSides (getInvalidRightSides rightSide') 2))
  -- check whether the final set of productions is not empty
  --  when (null $ zip leftSide rightSide') $ exitWithErrMsg (ExitFailure 1) (missingSymbols isSpace)
  -- ^ return the validate set of productions
  return $ nub $ zip leftSide rightSide'
  where
    splittedProductions = map (span (/= '-') . strip) productions -- ^ strip production to L and ->R
    leftSide = map (strip . fst) splittedProductions -- ^ L from L->R
    rightSide = map (strip . snd) splittedProductions -- ^ R from L->R
    arrowIndices = map (findString "->") rightSide -- ^ location of the arrow in the production
    -- ^ order number of productions that have the invalid arrows
    errArrowIndices = [0 .. genericLength productions - 1] \\ elemIndices 0 arrowIndices
    -- ^ package the invalid left sides od productions
    invalidLeftSides = zip errArrowIndices (filter (/= 0) arrowIndices)