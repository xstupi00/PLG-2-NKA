module GrammarControl where

import ErrorControl
import Grammar
import Helpers

import Control.Monad
import Data.Char
import Data.List
import System.Exit

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
        (findIndices (`elem` invalidProductions) (productions' grammar))
        invalidProductions
        invalidProductionsCodes

missingStartSymbol :: Grammar -> Bool
missingStartSymbol grammar = startSymbol' grammar `notElem` variables' grammar

missingRuleWithStartSymbol :: Grammar -> Bool
missingRuleWithStartSymbol grammar =
  not (any (\(x, _) -> x == startSymbol' grammar) (productions' grammar))

filterProductions :: Grammar -> [(String, String)]
filterProductions grammar =
  products \\
  (epsilonProductions `union` simpleProductions `union` terminalProductions `union` rightProductions)
  where
    epsilonProductions = filterEpsilonProductions products vars terms
    simpleProductions = filterSimpleProductions products vars terms
    terminalProductions = filterTerminalProductions products vars terms
    rightProductions = filterRightProductions products vars terms
    products = productions' grammar
    vars = variables' grammar
    terms = terminals' grammar

filterEpsilonProductions :: (Foldable t, Eq a) => [(a, String)] -> t a -> p -> [(a, String)]
filterEpsilonProductions products vars terms =
  filter (\(l, r) -> l `elem` vars && length r == 1 && head r == '#') products

filterSimpleProductions ::
     (Eq a, Foldable t1, Foldable t2) => [([a], [a])] -> t1 [a] -> t2 [a] -> [([a], [a])]
filterSimpleProductions products vars terms =
  filter
    (\(l, r) -> l `elem` vars && length r == 2 && [head r] `elem` terms && [last r] `elem` vars)
    products

filterTerminalProductions ::
     (Foldable t1, Foldable t2, Foldable t3, Eq a1, Eq a2)
  => [(a1, t2 a2)]
  -> t1 a1
  -> t3 [a2]
  -> [(a1, t2 a2)]
filterTerminalProductions products vars terms =
  filter (\(l, r) -> l `elem` vars && all (\ch -> [ch] `elem` terms) r) products

filterRightProductions ::
     (Eq a, Foldable t1, Foldable t2) => [([a], [a])] -> t1 [a] -> t2 [a] -> [([a], [a])]
filterRightProductions products vars terms =
  filter
    (\(l, r) ->
       l `elem` vars &&
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
    vars = variables' grammar
    terms = terminals' grammar