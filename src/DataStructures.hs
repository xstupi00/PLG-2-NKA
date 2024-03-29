{-|
Module      : DataStructures
Description : Definition of data structures
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module contains definition of data structures to
store the grammars and finite machine types.
-}
module DataStructures where

-- ^ Data structure to represent the grammar
data Grammar =
  Grammar -- ^ G = (V, T, S, P)
    { variables :: [String] -- ^ V 
    , terminals :: [String] -- ^ T
    , startSymbol :: String -- ^ S
    , productions :: [(String, String)] -- ^ P
    }
  deriving (Show)

-- ^ Data structure to represent finite automaton
data FiniteAutomaton =
  FiniteAutomaton -- ^ A = (Q, q0, F, delta) + sigma (input alphabet)
    { states :: [Integer] -- ^ Q
    , startingState :: Integer -- ^ q0
    , finalStates :: [Integer] -- ^ F
    , transitionFunction :: [(Integer, String, Integer)] -- ^ delta : Q x sigma -> Q
    }