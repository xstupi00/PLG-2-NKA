module MainControl where

import Grammar

import Data.List

printGrammar grammar = do
  putStrLn $ intercalate "," $ variables' grammar
  putStrLn $ intercalate "," $ terminals' grammar
  putStrLn $ startSymbol' grammar
  putStrLn $ intercalate "\n" $ map (\(l, r) -> l ++ "->" ++ r) (productions' grammar)