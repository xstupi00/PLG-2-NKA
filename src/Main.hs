module Main where

import Grammar
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
  when (TLG `elem` args) $ transformGrammar grammar