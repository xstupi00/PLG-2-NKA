import System.Random
import System.IO

data DLog
  = Empty
  | Record Integer String
  deriving (Show, Eq)

--pt :: String -> IO [DLog]
pt filename = do
  content <- readFile filename
  let contentList = lines content
  let newRecords = map parse contentList
  let dlog = map (\(x,y) -> if ((x/="") && (y/="")) then Record (read x::Integer) y else Empty) newRecords
  let filterdlog = filterLog dlog
  let strlog = printlog filterdlog
  putStr strlog
    where
      parse record = if record == "" then ("", "") else let (num, (_:str)) = span (/='#') record in (num, str)
      filterLog log = [(Record x y) | (Record x y) <- log, x `mod` 5 == 0]

printlog [] = []
printlog ((Record x y):records) = (show x)++":"++y++"\n" ++ printlog records


createPrimeList = [x | x <- [2,3..], isPrime x (x-1)]

isPrime _ 1 = True
isPrime x n = x `mod` n /= 0 && isPrime x (n - 1)

--mkr filename = do
--  content <- readFile filename
--  let flines = lines content
--  let logins = getLogins flines
--  let texts = getTexts flines
--  let empty = filter (\l -> l == "") flines
--  putStrLn $ show $ length(logins)
--  putStrLn $ show $ length(texts)
--  putStrLn $ show $ length(empty)
--  let randomLogins = commonLogins logins
--  return randomLogins
--
--commonLogins [] = return []
--commonLogins logins = do
-- idx <- randomRIO (0, (length(logins) - 1))
-- (logins !! idx) ++ commonLogins (deleteL idx logins 0)
--
--deleteL _ [] 0 = []
--deleteL idx (a:as) order =
--  if idx == order then (deleteL idx as (order+1)) else [a] ++ (deleteL idx as (order+1))
--
--getTexts [] = []
--getTexts (a:as) = if isText a then [a] ++ getTexts as else getTexts as
--  where isText text = text/="" && all (\c -> c `elem` ['A'..'z'] || c `elem` ['0'..'9']) text

getLogins [] = []
getLogins (a:as) = if isLogin' a then [a] ++ getLogins as else getLogins as
  where isLogin' login = if length(login) == 8 then isLogin login else False
        isLogin (a:as) =
          a == 'x' && all (\c -> c `elem` ['a'..'z']) (take 5 as)
          && (as !! 5) `elem` ['0'..'9'] && (as !! 6) `elem` ['0'..'9']

fdup file = do
  h <- openFile file ReadMode
  c <- hGetContents h
  let l = lines c
  let o = createOutput l
  putStr o
  hClose h

createOutput [] = ""
createOutput (('+':'+':xs):ys) = xs++"\n"++xs++"\n" ++ createOutput ys 
createOutput (xs:ys) = xs++"\n" ++ createOutput ys