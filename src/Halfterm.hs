module Halfterm where

import System.IO

printFile :: Int -> [String] -> [String] -> [String]
printFile _ [] [] = []
printFile line (x:xs) [] = (show line ++ ": " ++ x) : printFile (line + 1) xs []
printFile line [] (y:ys) = (show line ++ ": " ++ y) : printFile (line + 1) [] ys
printFile line (x:xs) (y:ys) =
  (show line ++ ": " ++ x) : (show (line + 1) ++ ": " ++ y) : printFile (line + 2) xs ys

printFile2 :: Int -> [String] -> [String]
printFile2 _ [] = []
printFile2 line [x] = [show line ++ ": " ++ x]
printFile2 line (x:_:xs) = (show line ++ ": " ++ x) : printFile2 (line + 2) xs

-- ways of splitting a list into two parts
splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (a:as) = ([], a : as) : [(a : bs, cs) | (bs, cs) <- splits as]

-- perms of a list
perms :: [a] -> [[a]]
perms (a:as) = [bs ++ a : cs | perm <- perms as, (bs, cs) <- splits perm]
perms [] = [[]]

takeN :: Int -> [a] -> [a]
takeN _ [] = []
takeN n (x:xs)
  | n <= 0 = []
  | otherwise = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a]
dropN _ [] = []
dropN n (x:xs)
  | n == 0 = x:xs
  | n < 0 = []
  | otherwise = dropN (n-1) xs

replaceN :: Eq a => Int -> a -> a -> [[a]] -> [[a]]
replaceN _ _ _ [] = []
replaceN n a b (x:xs) = (take n x ++ replace (dropN n x)) : replaceN n a b xs
  where
    replace (y:ys) =
      if y == a
        then b : ys
        else y : ys
    replace [] = [] 
    
splitAtN :: Int -> [a] -> ([a], [a])
splitAtN n xs = (takeN n xs, dropN n xs) 
 
main :: IO ()
main = do
  putStrLn "Priklad 1:"
  handle1 <- openFile "./test/valid_tests/empty_language.in" ReadMode
  handle2 <- openFile "./test/valid_tests/epsilons.in" ReadMode
  content1 <- hGetContents handle1
  content2 <- hGetContents handle2
  putStr $ unlines $ printFile 0 (lines content1) (lines content2)
  putStrLn "-------------------------------"
  putStrLn $ "file1: " ++ show (length content1)
  putStrLn $ "file2: " ++ show (length content2)
  putStr $
    unlines $
    printFile2 1 $
    if length content1 < length content2
      then lines content1
      else lines content2
  putStrLn "-------------------------------"
  writeFile "./test/test.in" (show $ replicate (length (lines content1)) "panda\n")
  putStrLn "-------------------------------"