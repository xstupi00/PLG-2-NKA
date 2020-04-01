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

-- ^ write out the grammar data structure according to required format on the STDOUT
printGrammar ::
     Grammar -- ^ given grammar structure to write out 
  -> IO () -- ^ write to the STDOUT
printGrammar grammar
  -- ^ variables separated by a comma - A,B,C,D, sorted alphabetically
 = do
  putStrLn $ intercalate "," $ sort $ variables grammar
  -- ^ terminals separated by a comma - a,b,c,b, sorted alphabetically
  putStrLn $ intercalate "," $ sort $ terminals grammar
  -- ^ write out the start symbol
  putStrLn $ startSymbol grammar
  -- ^ write out the productions on the individual line in the form - A->aB, sorted alphabetically
  putStrLn $ intercalate "\n" $ sort $ map (\(l, r) -> l ++ "->" ++ r) (productions grammar)

-- ^ write out the finite automaton data structure according to specification to the STDOUT
printFiniteAutomaton ::
     FiniteAutomaton -- ^  given finite automaton to write out
  -> IO () -- ^ write to the STDOUT
printFiniteAutomaton finiteAutomaton
  -- ^ states separated by a comma - 0,1,2,3
 = do
  putStrLn $ intercalate "," $ map show $ states finiteAutomaton
  -- ^ write out the starting state
  print (startingState finiteAutomaton)
  -- ^ final states separated by a comma - 2,3
  putStrLn $ intercalate "," $ map show $ finalStates finiteAutomaton
  -- ^ transition function at the separated line in the form - 0,a,1
  putStrLn $
    intercalate "\n" $
    sort $
    map (\(x, y, z) -> show x ++ "," ++ y ++ "," ++ show z) (transitionFunction finiteAutomaton)

-- ^ check whether the grammar not generate the empty language and then transform it
transformGrammar :: Grammar -> IO Grammar
transformGrammar grammar
  -- ^ when the grammar generates empty language then we return the trivial language
  | isLanguageOfGrammarEmpty grammar =
    return
      Grammar
        { variables = [startSymbol'] -- ^ S from G, only needed variable
        , terminals = [] -- ^ empty language -> no needed terminals
        , startSymbol = startSymbol' -- ^ S from G, remains unchanged
        , productions = [(startSymbol', startSymbol')] -- ^ S->S (empty language of new G)
        }
  -- ^ when the language is non-empty then run the process of transformation
  | otherwise = transformGrammar' grammar
  where
    startSymbol' = startSymbol grammar -- ^ S from G

-- ^ transform given grammar to the specified format according to the specification
transformGrammar' :: Grammar -> IO Grammar
transformGrammar' grammar
  -- ^ create the starting tuple map for each variable with zero count -  [(A, 0), (B, 0), ...]
 = do
  let variablesCountInit = zip vars (replicate (length vars) 0)
  -- ^ transform right productions to the required format
  -- ^ A->ab..cD => A->aA0, A0->bA1, A1->cD
  let (variablesCountRight, rightProductions) =
        transformProductions True (filterRightProductions products vars terms) variablesCountInit
  -- ^ transform terminal productions to the required format
  -- ^ A->ab..c => A->aA2, A2->bA3, A3->cA4, A4-># 
  let (variablesCountTerminal, terminalProductions) =
        transformProductions
          False
          (filterTerminalProductions products vars terms)
          variablesCountRight
  -- ^ create the set of productions for new transformed grammar
  let newProductions =
        epsilonProductions `union` basicProductions `union` rightProductions `union`
        terminalProductions
  -- ^ transform simple productions from the original grammar according to yet transformed
  let transformedSimpleProductions = transformSimpleProductions simpleProductions newProductions
  -- ^ add transformed simple productions to the final set of productions for the new grammar
  let finalProductions = newProductions `union` transformedSimpleProductions
  -- ^ construct the transformed grammar data structure 
  return
    Grammar
      { variables = nub $ union (variables grammar) $ map fst finalProductions -- ^ V
      , terminals = nub $ terminals grammar -- ^ T
      , startSymbol = startSymbol grammar -- ^ S
      , productions = nub finalProductions -- ^ P
      }
  where
    epsilonProductions = filterEpsilonProductions products vars -- ^ A->#
    basicProductions = filterBasicProductions products vars terms -- ^ A->ab
    simpleProductions = filterSimpleProductions products vars -- ^ A->B
    products = productions grammar -- ^ P
    vars = variables grammar -- ^ V
    terms = terminals grammar -- ^ T

-- ^ transform given grammar to the finite automaton according to the given algorithm
transformGrammarToNFA :: Grammar -> FiniteAutomaton
transformGrammarToNFA grammar =
  FiniteAutomaton
      -- | map the set of variables to the set of states - A->0, B->1, ..., C->n 
    { states = [0 .. genericLength (variables grammar) - 1]
    -- | obtaining the start symbol and the check the its relevant state
    , startingState = getStateFromVariable $ startSymbol grammar
    -- | construct the set of final states according to epsilon productions
    , finalStates = map getStateFromVariable getEpsilonVariables
    -- | construct the transition function according to the set of productions from the grammar
    , transitionFunction = map transformProduction getTransitionProductions
    }
  where
    varsMapToStates = zip (sort $ variables grammar) [0 ..] -- ^ [(A, 0), (B, 1), ..., (C, n)]
    -- ^ get relevant state according to the given variable - for A return 0
    getStateFromVariable variable =
      snd $ head $ filter (\(var, _) -> var == variable) varsMapToStates
    -- ^ return all epsilon variables, thus find the production A-># for all A in V
    getEpsilonVariables = map fst $ filter (\(_, r) -> r == "#") $ productions grammar
    -- ^ return all production which have to transform to the transition function
    getTransitionProductions = filter (\(_, r) -> r /= "#" && length r > 1) $ productions grammar
    -- ^ transform given production to the format required by NFA
    transformProduction (l_var, term:r_var) =
      (getStateFromVariable l_var, [term], getStateFromVariable r_var)