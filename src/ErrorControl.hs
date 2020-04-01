{-|
Module      : ErrorControl
Description : Controller for occurrence of the error states.
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module contains error strings to write out when
some error state was occurred and also it contains
the auxiliary functions to construct the composed error
strings in some aggregated situations.
-}
module ErrorControl where

import System.Exit
import System.IO

missingContent :: String -- ^ error string
missingContent = "Missing content of given input."

errMsgGrammar :: String -- ^ error string
errMsgGrammar = "Wrong format of input grammar:\n "

varErrMsgRange :: String -- ^ error string
varErrMsgRange = "Variables must be character in the range [A-Z]: "

varErrMsgOccur :: String -- ^ error string
varErrMsgOccur = "Variables should no be entered twice in the input list: "

termErrMsgRange :: String -- ^ error string
termErrMsgRange = "Terminals must be character in the range [a-z]: "

termErrMsgOccur :: String -- ^ error string
termErrMsgOccur = "Terminals should not be entered twice in the input list: "

productionsWrongFormat :: String -- ^ error string
productionsWrongFormat = "Productions have not required format:\n"

-- ^ Return the relevant error string according to given classification function
missingSymbols ::
     (Char -> Bool) -- ^ character classification function (isAsciiLower, isAsciiUpper, isSpace)
  -> String -- ^ error string to write out
missingSymbols group
  -- | isAsciiLUpper to validate variables
  | group 'A' = "Empty set of variables on the line no. 1."
  -- | isAsciiLower to validate terminal symbols
  | group 'a' = "Empty set of terminals on the line no. 2."
  -- | missing productions on the relevant lines
  | otherwise = "Empty set of productions on the line no. 4."

-- ^ return error string according to given error code to recognize different error states
invalidStartSymbol ::
     Int -- ^ error code to recognize different error states
  -> String -- ^ error string to write out
invalidStartSymbol code
  -- | given start symbol is missing between variables
  | code == 0 = errMsgGrammar ++ "The start symbol must be included between variables."
  -- | given start symbol is not included in some production on the right side
  | code == 1 =
    errMsgGrammar ++
    "The start symbol must be included at least one in production on the left side."
  -- | missing start symbol
  | code == 2 = errMsgGrammar ++ "Missing start symbol at the line no. 4."
  -- | generic error due to total function
  | otherwise = errMsgGrammar

-- ^ exit program with given exit code and write out given error string to stderr
exitWithErrMsg ::
     ExitCode -- ^ program exit with this exit code
  -> String -- ^ error message to write out 
  -> IO a -- ^ exit program
exitWithErrMsg errCode errMsg = hPutStrLn stderr errMsg >> exitWith errCode

-- ^ print error string to stderr
printWarning ::
     String -- ^ error message to write out
  -> IO () -- ^ write out to stderr
printWarning = hPutStrLn stderr

-- ^ write out the error messages according to error specified invalid productions
invalidProductionsErrMsg ::
     [(Int, (String, String), (Int, String))] -- ^ [(line, production, (err code, invalid symbol))] 
  -> String -- ^ return error string to write out
invalidProductionsErrMsg errTuples
  -- ^ generic error string at invalid productions
 =
  baseErrMsg ++
  "   Invalid format of productions no.: " ++
  -- ^ write out the information stored in the list of error tuples
  show
    (map
       (\(x, (y, z), (c, vars)) -> show (x + 4) ++ ". line: " ++ y ++ "->" ++ z ++ errMsg c vars)
       errTuples)
  where
    baseErrMsg = errMsgGrammar ++ productionsWrongFormat -- ^ generic error message
    errMsg code vars -- ^ error code and invalid variables
      -- | any variables in the production missing in the defined variables
      | code == 0 = " (missing variable(s): " ++ vars ++ ")"
      | code == 1 = " (missing variable(s): " ++ vars ++ ")"
      -- | any terminals in the production missing in the defined terminals
      | code == 2 = " (missing terminal(s): " ++ vars ++ ")"
      -- | the defined production is not in the required format
      | otherwise = " (no right linear grammar)"

-- ^ create the error specific message according to the detected error state in the productions
productionErrMsg ::
     [(Int, Int)] -- ^ number of line and column where the error was occurred
  -> [(Int, String)] -- ^ number of line and invalid part of production
  -> Int -- ^ error code to recognize different error states
  -> String -- ^ error string to write out
productionErrMsg errIndices errTuples errCode
  -- ^ invalid location of the arrows in the production -> create relevant error string
  | errCode == 0 =
    baseErrMsg ++
    "   Missing or badly located arrows in productions no.: " ++
    show (map (\(x, y) -> show (x + 4) ++ ". line: " ++ show (y + 1) ++ ". column") errIndices)
  -- ^ invalid left side of the production (not variable) -> create relevant error string
  | errCode == 1 =
    baseErrMsg ++
    "   Left side of productions must be character in the range [A-Z]: " ++
    show (map (\(x, y) -> show (x + 4) ++ ". line: " ++ y) errTuples)
  -- ^ invalid right side of the production -> create relevant error string
  | errCode == 2 =
    baseErrMsg ++
    "   Invalid format of right side in productions no.: " ++
    show (map (\(x, y) -> show (x + 4) ++ ". line: " ++ y) errTuples)
  -- | generic error due to total function
  | otherwise = baseErrMsg
  where
    baseErrMsg = errMsgGrammar ++ productionsWrongFormat -- ^ generic error message

-- ^ construct the error message at invalid symbols
symbolErrMsg :: (Show a1, Show a2) => Bool -> [(String, a2)] -> a1 -> (Char -> Bool) -> String
symbolErrMsg isRange multipleSymbols wrongSymbols symbolGroup =
  errMsgGrammar ++
  if symbolGroup 'A'
    then if isRange
           then varErrMsgRange ++ show wrongSymbols
           else varErrMsgOccur ++ show (map (\(x, y) -> x ++ ": " ++ show y) multipleSymbols)
    else if isRange
           then termErrMsgRange ++ show wrongSymbols
           else termErrMsgOccur ++ show (map (\(x, y) -> x ++ ": " ++ show y) multipleSymbols)