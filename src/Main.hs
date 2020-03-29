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

main :: IO ()
main = do
  (args, files) <- parseArgs
  grammar <- parseInput $ head files
  validateGrammar grammar
  when (PLG `elem` args) $ printGrammar grammar
  transformedGrammar <- transformGrammar grammar
  when (TLG `elem` args) $ printGrammar transformedGrammar
  when (NKA `elem` args) $ printFiniteAutomaton $ transformGrammarToNFA transformedGrammar