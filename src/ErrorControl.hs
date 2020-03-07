module ErrorControl where

import Data.List
import System.Exit
import System.IO

missingContent :: String
missingContent = "Missing content of given input."

errMsgGrammar :: String
errMsgGrammar = "Wrong format of input grammar: \n "

varErrMsgRange :: String
varErrMsgRange = "Variables must be character in the range [A-Z]: "

varErrMsgOccur :: String
varErrMsgOccur = "Variables should no be entered twice in the input list: "

termErrMsgRange :: String
termErrMsgRange = "Terminals must be character in the range [a-z]: "

termErrMsgOccur :: String
termErrMsgOccur = "Terminals should not be entered twice in the input list: "

productionsWrongFormat :: String
productionsWrongFormat = "Productions have not required format: \n"

missingSymbols :: (Char -> Bool) -> String
missingSymbols group
  | group 'A' = "Empty set of variables on the line no. 1."
  | group 'a' = "Empty set of terminals on the line no. 2."
  | otherwise = "Empty set of productions on the line no. 4."
 
exitWithErrMsg :: ExitCode -> String -> IO a
exitWithErrMsg errCode errMsg = hPutStrLn stderr errMsg >> exitWith errCode

printWarning :: String -> IO ()
printWarning = hPutStrLn stderr

productionErrMsg :: [(Int, Int)] -> [(Int, String)] -> Int -> String
productionErrMsg errIndices errTuples errCode
  | errCode == 0 =
    baseErrMsg ++
    "   Missing or badly located arrows in productions no.: " ++
    show (map (\(x, y) -> show (x + 4) ++ ". line: " ++ show (y + 1) ++ ". column") errIndices)
  | errCode == 1 =
    baseErrMsg ++
    "   Left side of productions must be character in the range [A-Z]: " ++
    show (map (\(x, y) -> show (x + 4) ++ ". line: " ++ y) errTuples)
  | errCode == 2 =
    baseErrMsg ++
    "   Invalid format of right side in productions no.: " ++
    show (map (\(x, y) -> show (x + 4) ++ ". line: " ++ y) errTuples)
  | otherwise = baseErrMsg
  where
    baseErrMsg = errMsgGrammar ++ productionsWrongFormat

symbolErrMsg isRange multipleSymbols wrongSymbols symbolGroup =
  errMsgGrammar ++
  if symbolGroup 'A'
    then if isRange
           then varErrMsgRange ++ show wrongSymbols
           else varErrMsgOccur ++ show (map (\(x, y) -> x ++ ": " ++ show y) multipleSymbols)
    else if isRange
           then termErrMsgRange ++ show wrongSymbols
           else termErrMsgOccur ++ show (map (\(x, y) -> x ++ ": " ++ show y) multipleSymbols)