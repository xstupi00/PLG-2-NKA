module GrammarControl where

import Helpers

import Data.List

removeEpsilon = map removeEpsilon'
  where
    removeEpsilon' production =
      if any isAsciiAlpha rightSide
        then (leftSide, remove "#" rightSide)
        else (leftSide, nub rightSide)
        where leftSide = fst production
              rightSide = snd production