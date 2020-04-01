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

import DataStructures

import Data.Char
import Data.List
import Data.Maybe

-- ^ remove the simple productions by replacing for a relevant transformed productions
transformSimpleProductions ::
     [(String, String)] -- ^ set of the filtered simple production in the form A->B
  -> [(String, String)] -- ^ set of the all transformed production in the previous steps
  -> [(String, String)] -- ^ set of the newly constructed productions based on the simple prods
transformSimpleProductions simple_productions productions
  -- ^ construct the new productions
 =
  concatMap
    (\(var, vars) -> zip (replicate (length $ rightSides vars) var) (rightSides vars))
    simpleSetOfVariables
  where
    variables = nub $ map fst simple_productions -- ^ left sides from the simple productions
    -- ^ create the simple set N_A for each variable from the grammar - N_A for each A in V
    simpleSetOfVariables =
      map (\var -> (var, getSimpleSet [var] [var] simple_productions)) variables
    -- ^ obtain all right sides of the given variable
    rightSides variable = getRightSides variable productions

-- ^ obtains all right sides of the given variables from the set of productions
getRightSides ::
     [String] -- ^ set of the the variables
  -> [(String, String)] -- ^ set of the productions
  -> [String] -- ^ set of the resulting right sides according to given variables
-- ^ end condition - all variables were researched
getRightSides [] _ = []
-- ^ get right sides of the current var and then recursively call to remaining variable
getRightSides (var:vars) productions = filterRightSides ++ getRightSides vars productions
  where
    filterRightSides = map snd $ filter (\(l, r) -> l == var) productions -- ^ obtain right sides

-- ^ construct the simple set for the given variable - N_A for each A from V
getSimpleSet ::
     [String] -- ^ already checked variables, at the begin {A} for set N_A 
  -> [String] -- ^ current simple set N_A
  -> [(String, String)] -- ^ the set of all productions from the grammar
  -> [String] -- ^ return the constructed simple set N_A
-- ^ end of the recursion when any other variable does not remains
getSimpleSet _ [] _ = []
-- ^ recursive call for each variable in the current simple set
-- ^ N_A = {B | A ⇒∗ B}
getSimpleSet checked_vars (variable:variables) productions =
  nub $
  getSimpleDerivations `union` [variable] ++
  getSimpleSet (checked_vars ++ [variable]) (getSimpleDerivations ++ variables) productions
  where
    getSimpleDerivations
      -- ^ obtains the all right sides from the productions, where matching the given variable
     = map snd $ filter (\(l, r) -> l == variable && r `notElem` checked_vars) productions

-- ^ transform the given productions to the required format
-- ^ isRight : A->ab..cB => A->aA0, A0->bA1, A1->cB
-- ^ ~ isRight : A->ab..c => A->aA2, A->bA3, A3->cA4, A4-># 
-- ^ returns the update map of variables and newly transformed productions
transformProductions ::
     Bool -- ^ flag to recognition the way of transformation
  -> [(String, String)] -- ^ productions for the transformation
  -> [(String, Int)] -- ^ map of variables to its number of derived variables
  -> ([(String, Int)], [(String, String)]) -- ^ updated map of variable and new productions
-- ^ end of the transformation and return the current map varCount 
transformProductions _ [] varCounts = (varCounts, [])
-- ^ perform the transformation of all productions in recursive way by symbols
transformProductions isRight (production:productions) varCounts
  -- ^ updated map varCount and transform current production ++ transformed remains productions
 = (fst transformedResult, transformProduction isRight index production ++ snd transformedResult)
  where
    index = snd $ head $ filter (\(x, _) -> x == [head $ fst production]) varCounts
    -- ^ update the map varCount according to the current transformed production
    updateVarCount =
      map
        (\(x, y) -- ^ (L, R) from L->R
          ->
           if x == [head $ fst production] -- ^ match with the current variable in te left side
             then ( x
                  , if isRight -- ^ check which way of transformation is currently performed
                      then y + productionLength - 2 -- ^ A->abB => A->aA0, A0->bB (3 - 2 = 1 -> A0)
                      else y + productionLength -- ^ A->ab => A->aA0, A0->bA1, A1-># (2 -> A0, A1)
                   )
             else (x, y) -- ^ when variable does not match, then without change
         )
        varCounts
    productionLength = length $ snd production -- ^ get length of the right side of the production
    transformedResult = transformProductions isRight productions updateVarCount

-- ^ perform the transformation of the production according to specification
transformProduction ::
     Bool -- ^ flag to recognize the form of the productions - A->abB (isRight) or A->ab (~isRight) 
  -> Int -- ^ starting index to create the new variables
  -> (String, String) -- ^ production to transform
  -> [(String, String)] -- ^ the set of the newly constructed productions
-- ^ end condition for production of the form A->ab, where is needed to add the last production
transformProduction False _ (l_var, []) = [(l_var, "#")]
-- ^ end condition for production of the form A->abB => l_var=A, term=b, var=B
transformProduction True _ (l_var, [term, var]) = [(l_var, term : [var])]
-- ^ isRight : A->abB => A->aA0, A0->bB (index = 0)
-- ^ ~ isRight : A->ab => A->aA1, A1->bA2, A2-># (index = 1)
transformProduction isRight index (l_var, term:symbols)
  -- ^ perform the transformation gradually by relevant symbols
 = (l_var, term : newVariable) : transformProduction isRight (index + 1) (newVariable, symbols)
  where
    newVariable = getNewVariable l_var index -- ^ create new variable : l_var=A, index=0 => A0

-- ^ construct the given variable according to the given variable and index
getNewVariable ::
     String -- ^ variable (e.g. A) 
  -> Int -- ^ index (e.g 0)
  -> String -- ^ new variable (e.g. A0)
getNewVariable variable idx = head variable : show idx