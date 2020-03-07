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
  [ Option ['i'] [] (NoArg PLG) "TODO DESCRIPTION"
  , Option ['1'] [] (NoArg TLG) "TODO DESCRIPTION"
  , Option ['2'] [] (NoArg NKA) "TODO DESCRIPTION"
  , Option [] ["help"] (NoArg Help) "Print this help message"
  ]

parseArgs = getArgs >>= parse

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