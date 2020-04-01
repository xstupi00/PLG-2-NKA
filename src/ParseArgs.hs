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

-- ^ supported options which can be entered in the command line when running the program
data Options
  = Help -- ^ --help - write the usage of the program
  | PLG -- ^ -i option -> write out loaded grammar from internal representation 
  | NKA -- ^ -1 option -> write out transformed grammar from the internal representation
  | TLG -- ^ -2 option -> write out constructed NFA created according to the given grammar
  deriving (Eq, Ord, Enum, Show, Bounded)

-- ^ supported option information structure
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

-- ^ wrapper function to call from main function of the program
parseArgs ::
     IO ([Options], [String]) -- ^ tuple of entered options and also the input files (arguments)
parseArgs = getArgs >>= parse

-- ^ parse the arguments and options entered in the command line
parse ::
     [String] -- ^ arguments from the command line 
  -> IO ([Options], [String]) -- ^ processed arguments and options
parse argv =
  case getOpt Permute options argv of
    (args, fs, [])
    -- ^ check whether was given some input file
     -> do
      let files =
            if null fs
              then ["-"]
              else fs
     -- ^ check whether was chosen help option to write out usage of the program
      if Help `elem` args
        then exitWithErrMsg ExitSuccess printUsage
        else return (nub args, files)
     -- ^ invalid option or arguments were entered
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ printUsage)
      exitWith $ ExitFailure 1
  where
    printUsage = usageInfo header options
    header = "Usage: plg-2-nka options [input]"