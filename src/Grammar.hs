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