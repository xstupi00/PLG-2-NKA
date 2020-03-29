{-|
Module      : ParseArgs
Description : Module for the processing of the arguments
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module contains the definition of the data structure
for available options and the function to parser the given
arguments from the command line.
-}
module ParseArgs where

import ErrorControl
import Helpers

import Data.List
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

data Options
  = Help -- --help
  | PLG -- -i
  | NKA -- -1
  | TLG -- -2
  deriving (Eq, Ord, Enum, Show, Bounded)

options =
  [ Option ['i'] [] (NoArg PLG) "Print out the loaded grammar from the internal representation"
  , Option
      ['1']
      []
      (NoArg TLG)
      "Print out the transformed grammar according to the given specification rules"
  , Option
      ['2']
      []
      (NoArg NKA)
      "Print out the NFA accepting the same language as the grammar in the input"
  , Option [] ["help"] (NoArg Help) "Print this help message and exit"
  ]

parseArgs :: IO ([Options], [String])
parseArgs = getArgs >>= parse

parse :: [String] -> IO ([Options], [String])
parse argv =
  case getOpt Permute options argv of
    (args, fs, []) -> do
      let files =
            if null fs
              then ["-"]
              else fs
      if Help `elem` args
        then exitWithErrMsg ExitSuccess printUsage
        else return (nub args, files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ printUsage)
      exitWith $ ExitFailure 1
  where
    printUsage = usageInfo header options
    header = "Usage: plg-2-nka options [input]"