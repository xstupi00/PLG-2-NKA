module TransformGrammar where

import Data.Char
import Data.List
import Data.Maybe

transformProductions ::
     Bool -> [(String, String)] -> [(String, Int)] -> ([(String, Int)], [(String, String)])
transformProductions _ [] varCounts = (varCounts, [])
transformProductions isRight (production:productions) varCounts =
  (fst transformedResult, transformProduction isRight index production ++ snd transformedResult)
  where
    index = snd $ head $ filter (\(x, _) -> x == [head $ fst production]) varCounts
    updateVarCount =
      map
        (\(x, y) ->
           if x == [head $ fst production]
             then ( x
                  , if isRight
                      then y + productionLength - 2
                      else y + productionLength)
             else (x, y))
        varCounts
    productionLength = length $ snd production
    transformedResult = transformProductions isRight productions updateVarCount

transformProduction :: Bool -> Int -> (String, String) -> [(String, String)]
transformProduction False _ (l_var, []) = [(l_var, "#")]
transformProduction True _ (l_var, [term, var]) = [(l_var, term : [var])]
transformProduction isRight index (l_var, term:symbols) =
  (l_var, term : newVariable) : transformProduction isRight (index + 1) (newVariable, symbols)
  where
    newVariable = getNewVariable l_var index

getNewVariable :: String -> Int -> String
getNewVariable variable idx = head variable : show idx