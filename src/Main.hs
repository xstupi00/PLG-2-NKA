module Main where

import DataStructures
import GrammarControl
import MainControl
import ParseArgs
import ParseInput
import Control.Monad


main = do
  (args, files) <- parseArgs
  grammar <- parseInput $ head files
  validateGrammar grammar
  when (PLG `elem` args) $ printGrammar grammar
  transformedGrammar <- transformGrammar grammar
  when (TLG `elem` args) $ printGrammar transformedGrammar
  when (NKA `elem` args) $ printFiniteAutomata $ transformGrammarToNFA transformedGrammar