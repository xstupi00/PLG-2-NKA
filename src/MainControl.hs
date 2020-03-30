{-|
Module      : MainControl
Description : Controller of the main functionality of the project
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module contains the function to run the main
functionality of the project. Specifically, it
contains the functions to print grammar and finite
machine and functions to transform grammar into
internal form and to the finite machine.
-}
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

printFiniteAutomaton :: FiniteAutomaton -> IO ()
printFiniteAutomaton finiteAutomaton = do
  putStrLn $ intercalate "," $ map show $ states finiteAutomaton
  print (startingState finiteAutomaton)
  putStrLn $ intercalate "," $ map show $ finalStates finiteAutomaton
  putStrLn $
    intercalate "\n" $
    sort $
    map (\(x, y, z) -> show x ++ "," ++ y ++ "," ++ show z) (transitionFunction finiteAutomaton)

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
  let newProductions =
        epsilonProductions `union` basicProductions `union` rightProductions `union`
        terminalProductions
  let transformedSimpleProductions = transformSimpleProductions simpleProductions newProductions
  let finalProductions = newProductions `union` transformedSimpleProductions
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
    simpleProductions = filterSimpleProductions products vars terms
    products = productions grammar
    vars = variables grammar
    terms = terminals grammar

transformGrammarToNFA :: Grammar -> FiniteAutomaton
transformGrammarToNFA grammar =
  FiniteAutomaton
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