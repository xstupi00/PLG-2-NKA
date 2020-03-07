module Main where

import ParseArgs
import ParseInput

main = do
  (args, files) <- parseArgs
  putStrLn $ "Flags: " ++ show args
  putStrLn $ "Files: " ++ show files
  input <- parseInput $ head files
  putStrLn "END OF PROGRAM "