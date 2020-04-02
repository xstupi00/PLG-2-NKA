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

-- ^ reduce epsilons in the epsilon production and remove in the non-epsilon productions
reduceEpsilon ::
     [(a, String)] -- ^ list of productions
  -> [(a, String)] -- ^ list of reduced production (remove epsilons)
reduceEpsilon = map reduceEpsilon'
  where
    reduceEpsilon' production
      -- ^ check whether the production contains some variables or terminals
     =
      if any isAsciiAlpha rightSide
        then (leftSide, remove "#" rightSide) -- ^ non-epsilon production -> remove epsilons (#)
        else (leftSide, nub rightSide) -- ^ epsilon production -> reduction to one epsilon (#)
      where
        leftSide = fst production -- ^ left side of the production (variable)
        rightSide = snd production -- ^ right side of the production

-- ^ the wrapper function to validate the given grammar
validateGrammar ::
     Grammar -- ^ loaded grammar from the given input 
  -> IO Grammar -- ^ exit program when the grammar is invalid or return validated grammar
validateGrammar grammar
  -- | check whether the start symbol was correctly entered
 = do
  when (missingStartSymbol grammar) $ exitWithErrMsg (ExitFailure 1) (invalidStartSymbol 0)
  -- | check whether was entered the production with the start symbol on right side
  when (missingRuleWithStartSymbol grammar) $ printWarning (invalidStartSymbol 1)
  -- | check whether the all productions has invalid format according to specification
  when (invalidProductions /= []) $
    exitWithErrMsg (ExitFailure 1) (invalidProductionsErrMsg invalidProductionsTuple)
  -- | return validate grammar if it is in requested format
  return grammar
  where
    invalidProductions = filterProductions grammar -- ^ obtaining the invalid productions
    -- ^ obtaining the specific error codes of the invalid productions
    invalidProductionsCodes = map (analyseInvalidProductions grammar) invalidProductions
    -- ^ create tuples with error productions, their error codes and their order number
    invalidProductionsTuple =
      zip3
        (findIndices (`elem` invalidProductions) (productions grammar))
        invalidProductions
        invalidProductionsCodes

-- ^ check whether the given variables include the given start symbol
missingStartSymbol ::
     Grammar -- ^ loaded grammar to obtain the start symbol
  -> Bool -- ^ Is the start symbol invalid?
missingStartSymbol grammar = startSymbol grammar `notElem` variables grammar

-- ^ check whether was given the production, which has on the right side the given start symbol
missingRuleWithStartSymbol ::
     Grammar -- ^ loaded grammar to obtain the start symbol
  -> Bool -- ^ Does not exists the production which has on the right side the given start symbol?
missingRuleWithStartSymbol grammar =
  not (any (\(x, _) -> x == startSymbol grammar) (productions grammar))

-- ^ filter all allowed forms of the production and then return the difference from original set
filterProductions ::
     Grammar -- ^ loaded grammar to obtain the productions 
  -> [(String, String)] -- ^ the list of filtered productions
filterProductions grammar =
  products \\
  (epsilonProductions `union` basicProductions `union` terminalProductions `union` rightProductions `union`
   simpleProductions)
  where
    epsilonProductions = filterEpsilonProductions products vars -- ^ A-># from P
    basicProductions = filterBasicProductions products vars terms -- ^ A->aB from P
    terminalProductions = filterTerminalProductions products vars terms -- ^ A->ab...c from P
    rightProductions = filterRightProductions products vars terms -- ^ A->ab...B from P
    simpleProductions = filterSimpleProductions products vars -- ^ A->B from P
    products = productions grammar -- ^ P from G
    vars = variables grammar -- ^ V from G
    terms = terminals grammar -- ^ T from G

-- ^ filter simple productions in the form A->B, where A, B are from V
filterSimpleProductions ::
     Eq t1
  => [([t1], [t1])] -- ^ productions from loaded grammar 
  -> [[t1]] -- ^ variables from loaded grammar 
  -> [([t1], [t1])] -- ^ the set of filtered simple productions
filterSimpleProductions products vars =
  filter (\(l, r) -> l `elem` vars && length r == 1 && [head r] `elem` vars) products

-- ^ filter epsilon productions in the form A->#, where is from V
filterEpsilonProductions ::
     Eq a
  => [(a, String)] -- ^ productions from loaded grammar 
  -> [a] -- ^ variables from loaded grammar
  -> [(a, String)] -- ^ the set of filtered epsilon productions
filterEpsilonProductions products vars =
  filter (\(l, r) -> l `elem` vars && length r == 1 && head r == '#') products

-- ^ filter basic productions in the form A->aB, where A,B are from V and a is from T
filterBasicProductions ::
     Eq t
  => [([t], [t])] -- ^ productions from loaded grammar 
  -> [[t]] -- ^ variables from loaded grammar
  -> [[t]] -- ^ terminals from loaded grammar
  -> [([t], [t])] -- ^ the set of filtered basic productions
filterBasicProductions products vars terms =
  filter
    (\(l, r) -> l `elem` vars && length r == 2 && [head r] `elem` terms && [last r] `elem` vars)
    products

-- ^ filter terminal productions in the form A->ab...c, where A is from V and a,b,c is from T
filterTerminalProductions ::
     (Eq a, Eq t)
  => [(a, [t])] -- ^ productions from loaded grammar 
  -> [a] -- ^ variables from loaded grammar
  -> [[t]] -- ^ terminals from loaded grammar
  -> [(a, [t])] -- ^ the set of filtered terminal productions
filterTerminalProductions products vars terms =
  filter (\(l, r) -> l `elem` vars && all (\ch -> [ch] `elem` terms) r) products

-- ^ filter right productions in the from A->ab...B, where A,B is from V and a,b,.. are from T
filterRightProductions ::
     Eq t
  => [([t], [t])] -- ^ productions from loaded grammar 
  -> [[t]] -- ^ variables from loaded grammar
  -> [[t]] -- ^ terminals from loaded grammar
  -> [([t], [t])] -- ^ the set of filtered basic productions
filterRightProductions products vars terms =
  filter
    (\(l, r) ->
       l `elem` vars &&
       length r > 2 &&
       all (\ch -> [ch] `elem` terms) (fst $ splitAt (length r - 1) r) && [last r] `elem` vars)
    products

-- ^ try to detect the error state in the invalid production
analyseInvalidProductions ::
     Grammar -- ^ loaded grammar to obtain sets of variables and terminals 
  -> (String, String) -- ^ set of invalid productions
  -> (Int, String) -- ^ return the detected error code and invalid symbol
analyseInvalidProductions grammar (left, right)
  -- | check whether the left side is within variables
  | left `notElem` vars = (0, left)
  -- | check whether all variables on the right side are within variables
  | any (\ch -> [ch] `notElem` vars) (filter isAsciiUpper right) =
    (1, filter (\ch -> [ch] `notElem` vars) (filter isAsciiUpper right))
  -- | check whether all terminals on the right side are within terminals
  | any (\ch -> [ch] `notElem` terms) (filter isAsciiLower right) =
    (2, filter (\ch -> [ch] `notElem` terms) (filter isAsciiLower right))
  -- | general case for total function
  | otherwise = (-1, "")
  where
    vars = variables grammar -- ^ V from G
    terms = terminals grammar -- ^ T from G

-- ^ check whether the given grammar generate the empty language
isLanguageOfGrammarEmpty ::
     Grammar -- ^ grammar data structure to check its language 
  -> Bool -- ^ Is language of grammar empty?
isLanguageOfGrammarEmpty grammar
  -- ^ when the startSymbol belong to final terminal set, then the language is non-empty
 = startSymbol grammar `notElem` constructTerminalSet grammar firstVarSet firstVarSet
  where
    firstVarSet = getTerminalSet grammar []
    -- ^ construct the set of the variables that generates terminal string in one step
    startSymbol' = startSymbol grammar -- ^ S from P

-- ^ construct the set of the terminals variables, thus they generate terminal string (N_t)
constructTerminalSet ::
     Grammar -- ^ grammar to obtain needed items to construct the set
  -> [String] -- ^ N_{i-1} \\ N_i -> check the end of the algorithm
  -> [String] -- ^ N_{i-1} -> terminal set from the previous iteration
  -> [String] -- ^ return constructed terminal set
-- ^ end condition to return the constructed terminal set
constructTerminalSet _ [] terminalSet = terminalSet
-- ^ construct the new terminal set in the current iteration
constructTerminalSet grammar diffSet terminalSet
  -- ^ recursive call of the potential next iteration
 = constructTerminalSet grammar (newVarSet \\ terminalSet) newVarSet
  where
    newVarSet = getTerminalSet grammar terminalSet -- ^ obtain N_i

-- ^ construct the current terminal set N_i - N_i = {A | A → α je v P ∧ α ∈ (N_{i−1} ∪ Σ)^∗}
getTerminalSet ::
     Grammar -- ^ grammar to obtain terminals and productions to construct the set 
  -> [String] -- ^ terminal set from the previous iteration N_{i-1}
  -> [String] -- ^ return current terminal set N_i
getTerminalSet grammar previousSet =
  nub $ -- ^ remove duplicated variable
  map fst $ -- ^ select only the left side from the production
  -- ^ match the production according to the definition of the terminal set
  filter
    (\(l, r) -> all (\ch -> [ch] `elem` (terminals grammar `union` previousSet `union` ["#"])) r) $
  productions grammar