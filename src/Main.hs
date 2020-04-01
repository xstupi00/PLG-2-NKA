{-|
Module      : Main
Description : Main module of the whole project
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

The main module that gradually runs the required
actions according to the given arguments, including
the parsing of these arguments.
-}
module Main where

import Control.Monad

import ErrorControl
import Helpers
import TransformGrammar

import DataStructures
import GrammarControl
import MainControl
import ParseArgs
import ParseInput

-- ^ main function of the whole program plg-2-nka
main :: IO ()
main = do
  -- ^ parse arguments from the command line
  (args, files) <- parseArgs
  -- ^ parse grammar from the given input file 
  grammar <- parseInput $ head files
  -- ^ check correctness of the loaded grammar
  validateGrammar grammar
  -- ^ option -i -> write out the loaded grammar from the internal representation
  when (PLG `elem` args) $ printGrammar grammar
  -- ^ construct the transformed grammar according to the given specification
  transformedGrammar <- transformGrammar' grammar
  -- ^ option -1 -> write out the transformed grammar from the internal representation 
  when (TLG `elem` args) $ printGrammar transformedGrammar
  -- ^ option -2 -> write out the constructed NFA with the same language as loaded grammar
  when (NKA `elem` args) $ printFiniteAutomaton $ transformGrammarToNFA transformedGrammar