{-|
Module      : TransformGrammar
Description : Module to transform the grammar into the specified format
Copyright   : (c) Simon Stupinsky, 2020
License     : GPL-3
Maintainer  : xstupi00@stud.fit.vutbr.cz
Project     : Functional project - plg-2-nka
Course      : Functional and Logic Programming (FLP)
University  : University of Technology Brno (BUT)
Faculty     : Faculty of Information Technology (FIT)

This module contains the functions to perform the 
transformation of the right linear grammar into
the specified format, from which then can be
constructed the NFA.
-}
module TransformGrammar where

import Data.Char
import Data.List
import Data.Maybe


transformSimpleProductions :: [(String, String)] -> [(String, String)] -> [(String, String)]
transformSimpleProductions simple_productions productions =
  concatMap
    (\(var, vars) -> zip (replicate (length $ rightSides vars) var) (rightSides vars))
    simpleSetOfVariables
  where
    variables = nub $ map fst simple_productions
    simpleSetOfVariables = map (\var -> (var, getSimpleSet [var] simple_productions)) variables
    rightSides variable = getRightSides variable productions

getRightSides :: [String] -> [(String, String)] -> [String]
getRightSides [] _ = []
getRightSides (var:vars) productions = filterRightSides ++ getRightSides vars productions
  where filterRightSides = map snd $ filter (\(l, r) -> l == var) productions

getSimpleSet :: [String] -> [(String, String)] -> [String]
getSimpleSet [] _ = []
getSimpleSet (variable:variables) productions =
  getSimpleDerivations ++ getSimpleSet (getSimpleDerivations++variables) productions
  where getSimpleDerivations = map snd $ filter (\(l, r) -> l == variable) productions

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