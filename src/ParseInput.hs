module ParseInput where

import ErrorControl
import Helpers
import ParseArgs

import Control.Monad
import Data.Char
import Data.List
import System.Exit

data Grammar =
  Grammar -- G = (V, T, S, P)
    { variables :: [String] -- V
    , terminals :: [String] -- T
    , startSymbol :: Char -- S
    , productions :: [(Char, String)] -- P
    }
  deriving (Show)

parseInput file = do
  grammar <-
    if file == "-"
      then getContents
      else readFile file
  variables <- validSymbols isAsciiUpper $ map strip $ splitBy ',' $ head $ lines grammar
  terminals <- validSymbols isAsciiLower $ map strip $ splitBy ',' $ (!! 1) $ lines grammar
  startSymbol <- validSymbols isAsciiUpper [strip ((!! 2) $ lines grammar)]
  productions <- validProductions $ drop 3 $ lines grammar
  putStrLn $ "Variables: " ++ show variables
  putStrLn $ "Terminals: " ++ show terminals
  putStrLn $ "startSymbol: " ++ head startSymbol
  putStrLn $ "productions: " ++ show productions

validSymbols symbolGroup symbols = do
  when
    (symbolTuples /= [])
    (printWarning $ symbolErrMsg False symbolTuples wrongSymbols symbolGroup)
  when
    (containsInvalidSymbol symbolGroup symbols || isNotGrammarSymbol symbols)
    (exitWithErrMsg (ExitFailure 1) $ symbolErrMsg True symbolTuples wrongSymbols symbolGroup)
  return $ nub symbols
  where
    symbolTuples = filter ((> 1) . snd) . map (\l@(x:xs) -> (x, length l)) . group . sort $ symbols
    wrongSymbols =
      (symbols \\ filter (all symbolGroup) symbols) `union` filter ((> 1) . length) symbols

validProductions productions = do
  when (getInvalidIndices productions /= []) $
    exitWithErrMsg
      (ExitFailure 1)
      (productionErrMsg (getInvalidArrows productions) (getInvalidLeftSides leftSide) 0)
  when
    (containsInvalidSymbol isAsciiUpper leftSide) -- || isNotGrammarSymbol rightSide
    (exitWithErrMsg
       (ExitFailure 1)
       (productionErrMsg (getInvalidArrows productions) (getInvalidLeftSides leftSide) 1))
  when
    (containsInvalidSymbols rightSide)
    (exitWithErrMsg
       (ExitFailure 1)
       (productionErrMsg (getInvalidArrows productions) (getInvalidRightSides rightSide) 2))
  return parsedProductions
  where
    parsedProductions = map (splitAt 1 . delete '-' . delete '>') productions
    leftSide = map fst parsedProductions
    rightSide = map snd parsedProductions