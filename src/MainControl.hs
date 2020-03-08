module MainControl where

import Grammar
import GrammarControl
import TransformGrammar

import Data.List

printGrammar :: Grammar -> IO ()
printGrammar grammar = do
  putStrLn $ intercalate "," $ sort $ variables grammar
  putStrLn $ intercalate "," $ sort $ terminals grammar
  putStrLn $ startSymbol grammar
  putStrLn $ intercalate "\n" $ sort $ map (\(l, r) -> l ++ "->" ++ r) (productions grammar)
  putStrLn "----------------------------------------------------------------------"

transformGrammar :: Grammar -> IO ()
transformGrammar grammar = do
  let variablesCountInit = zip vars (replicate (length vars) 0)
  let (variablesCountRight, rightProductions) = transformProductions True (filterRightProductions products vars terms) variablesCountInit
  let (variablesCountTerminal, terminalProductions) = transformProductions False (filterTerminalProductions products vars terms) variablesCountRight
  let finalProductions = epsilonProductions `union` basicProductions `union` rightProductions `union` terminalProductions
  printGrammar
    Grammar
      { variables = nub $ map fst finalProductions
      , terminals = terminals grammar
      , startSymbol = startSymbol grammar
      , productions = finalProductions
      }
  where
    epsilonProductions = filterEpsilonProductions products vars terms
    basicProductions = filterBasicProductions products vars terms
    products = productions grammar
    vars = variables grammar
    terms = terminals grammar