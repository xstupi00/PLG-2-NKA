module MainControl where

import DataStructures
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

printFiniteAutomata :: FiniteAutomata -> IO ()
printFiniteAutomata finiteAutomata = do
  putStrLn $ intercalate "," $ map show $ states finiteAutomata
  print (startingState finiteAutomata)
  putStrLn $ intercalate "," $ map show $ finalStates finiteAutomata
  putStrLn $
    intercalate "\n" $
    sort $
    map (\(x, y, z) -> show x ++ "," ++ y ++ "," ++ show z) (transitionFunction finiteAutomata)

transformGrammar :: Grammar -> IO Grammar
transformGrammar grammar = do
  let variablesCountInit = zip vars (replicate (length vars) 0)
  let (variablesCountRight, rightProductions) =
        transformProductions True (filterRightProductions products vars terms) variablesCountInit
  let (variablesCountTerminal, terminalProductions) =
        transformProductions
          False
          (filterTerminalProductions products vars terms)
          variablesCountRight
  let finalProductions =
        epsilonProductions `union` basicProductions `union` rightProductions `union`
        terminalProductions
  return
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

transformGrammarToNFA :: Grammar -> FiniteAutomata
transformGrammarToNFA grammar =
  FiniteAutomata
    { states = [0 .. genericLength (variables grammar) - 1]
    , startingState = getStateFromVariable $ startSymbol grammar
    , finalStates = map getStateFromVariable getEpsilonVariables
    , transitionFunction = map transformProduction getTransitionProductions
    }
  where
    varsMapToStates = zip (variables grammar) [0 ..]
    getStateFromVariable variable =
      snd $ head $ filter (\(var, _) -> var == variable) varsMapToStates
    getEpsilonVariables = map fst $ filter (\(_, r) -> r == "#") $ productions grammar
    getTransitionProductions = filter (\(_, r) -> r /= "#") $ productions grammar
    transformProduction (l_var, term:r_var) =
      (getStateFromVariable l_var, [term], getStateFromVariable r_var)