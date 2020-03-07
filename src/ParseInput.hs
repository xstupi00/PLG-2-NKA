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
  variables <- validateSymbols isAsciiUpper $ map strip $ splitBy ',' $ head $ lines grammar
  terminals <- validateSymbols isAsciiLower $ map strip $ splitBy ',' $ (!! 1) $ lines grammar
  startSymbol <- validateSymbols isAsciiUpper [strip ((!! 2) $ lines grammar)]
  productions <- validateProductions $ map strip $ drop 3 $ lines grammar
  putStrLn $ "Variables: " ++ show variables
  putStrLn $ "Terminals: " ++ show terminals
  putStrLn $ "startSymbol: " ++ head startSymbol
  putStrLn $ "productions: " ++ show productions

validateSymbols symbolGroup symbols = do
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

validateProductions productions = do
  when (errArrowIndices /= []) $
    exitWithErrMsg
      (ExitFailure 1)
      (productionErrMsg invalidLeftSides (getInvalidRightSides rightSide) 0)
  let rightSide' = map (strip . snd . splitAt 2) rightSide
  when
    (containsInvalidSymbol isAsciiUpper leftSide) -- || isNotGrammarSymbol rightSide
    (exitWithErrMsg
       (ExitFailure 1)
       (productionErrMsg invalidLeftSides (getInvalidLeftSides leftSide) 1))
  when
    (containsInvalidSymbols rightSide')
    (exitWithErrMsg
       (ExitFailure 1)
       (productionErrMsg invalidLeftSides (getInvalidRightSides rightSide') 2))
  return $ zip leftSide rightSide'
  where
    splittedProductions = map (span (/= '-') . strip) productions
    leftSide = map (strip . fst) splittedProductions
    rightSide = map (strip . snd) splittedProductions
    arrowIndices = map (findString "->") rightSide
    errArrowIndices = [0 .. genericLength productions - 1] \\ elemIndices 0 arrowIndices
    invalidLeftSides = zip errArrowIndices (filter (/= 0) arrowIndices)