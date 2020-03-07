module Grammar where

import Helpers

import Data.List

data Grammar =
  Grammar -- G = (V, T, S, P)
    { variables :: [String] -- V
    , terminals :: [String] -- T
    , startSymbol :: String -- S
    , productions :: [(String, String)] -- P
    }
  deriving (Show)
  
variables' :: Grammar -> [String] 
variables' (Grammar variables _ _ _) = variables

terminals' :: Grammar -> [String] 
terminals' (Grammar _ terminals _ _) = terminals

startSymbol' :: Grammar -> String 
startSymbol' (Grammar _ _ startSymbol _) = startSymbol

productions' :: Grammar -> [(String, String)] 
productions' (Grammar _ _ _ productions) = productions