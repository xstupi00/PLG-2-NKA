module DataStructures where

import Helpers

import Data.List

data Grammar =
  Grammar -- G = (V, T, S, P)
    { variables :: [String] -- V
    , terminals :: [String] -- T
    , startSymbol :: String -- S
    , productions :: [(String, String)] -- P
    }
  deriving (Show)

data FiniteAutomata =
  FiniteAutomata -- A = (Q, q0, F, delta)
    { states :: [Integer] -- Q
    , startingState :: Integer -- q0
    , finalStates :: [Integer] -- F
    , transitionFunction :: [(Integer, String, Integer)]
    }