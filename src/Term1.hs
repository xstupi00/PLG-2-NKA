import System.IO
import System.Random

data DirTree a
  = Dir String Integer a [DirTree a]
  | File String Integer a
  deriving(Show, Eq)

getSize :: DirTree a -> Integer
getSize(Dir name size _ dirs) = (+) size $ sum $ map getSize dirs
getSize(File string size _) = size

getPrefixList :: String -> DirTree a -> [String]
getPrefixList prefix dirTree = constructPrefixList prefix "" dirTree

constructPrefixList :: String -> String -> DirTree a -> [String]
constructPrefixList prefix path (File name _ _) =
  if cmpPrefix prefix name then [path ++ name] else []
constructPrefixList prefix path (Dir name _ _ dirs) =
  (if cmpPrefix prefix name then [path ++ name ++ "/"] else []) ++
    concatMap (constructPrefixList prefix (path ++ name ++ "/"))  dirs

cmpPrefix :: String -> String -> Bool
cmpPrefix _ [] = False
cmpPrefix [] _ = True
cmpPrefix (x:xs) (y:ys) = x==y && cmpPrefix xs ys

writeDir :: DirTree a -> IO ()
writeDir dirTree = do
  let dirString = constructString 0 dirTree
  putStr dirString
  putStrLn " + . \n  ."

constructString :: Int -> DirTree a -> String
constructString level (File name size _)  =
  getStar ++ " " ++ name ++ " " ++ (show size) ++ "\n"
    where getStar = concat $ take level $ repeat " +"
constructString level (Dir name size _ dirs) =
  getStar ++ (if level==0 then "-* " else "-* ") ++ name ++ "\n" ++
    concatMap (constructString (level + 1)) dirs
      where getStar = concat $ take level $ repeat " +"

data Dlog
  = Empty
  | Record Integer String
  deriving(Eq, Show)

pt :: String -> IO ()
pt filename = do
  h <- openFile filename ReadMode
  c <- hGetContents h
  let input = lines c
  let dlogs = parseInput input
  let parsedlogs = parseLogs dlogs
  putStr parsedlogs
  hClose h

parseLogs :: [Dlog] -> String
parseLogs [] = ""
parseLogs (Empty:logs) = parseLogs logs
parseLogs ((Record num str):logs) =
  if num `mod` 5 == 0 then (show num) ++ ":" ++ str ++ "\n" ++ parseLogs logs else parseLogs logs

parseInput :: [String] -> [Dlog]
parseInput [] = []
parseInput (line:lines) =
  (if line == "" then [Empty] else [Record (read num::Integer) string]) ++ parseInput lines
    where num = fst $ span (/='#') line
          string = snd $ span (/='#') line

data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving(Eq, Show)

initTree :: a -> Tree a
initTree val = Node val (initTree val) (initTree val)

takeLevel :: Int -> Tree a -> Tree a
takeLevel 0 _ = Leaf
takeLevel _ Leaf = Leaf
takeLevel lvl (Node val leftTree rightTree) =
  Node val (takeLevel (lvl - 1) leftTree) (takeLevel (lvl - 1) rightTree)

createPrimeList :: [Integer]
createPrimeList = [2] ++ filter (\x -> isPrime x [3,4..x-1]) [3,5..]

isPrime :: Integer -> [Integer] -> Bool
isPrime _ [] = True
isPrime num (n:n1) = (num `mod` n /= 0) && (isPrime num n1)

mkR :: String -> IO ()
mkR filename = do
  h <- openFile filename ReadMode
  c <- hGetContents h
  let input = lines c
  let emptyLines = length $  filter (\l -> l == "") input
  let textLines = length $ filter (\l -> l /= "") input
  let loginLines = filter (\l -> isLogin l) input
  putStrLn $ show $ length loginLines
  putStrLn $ show textLines
  putStrLn $ show emptyLines
  randLogins <- genRandLogins loginLines
  putStr $ unlines randLogins
  hClose h

genRandLogins :: [String] -> IO [String]
genRandLogins [] = return []
genRandLogins logins = do
  ir <- randomRIO (0, (length logins) - 1) :: IO Int
  let (headLogins, tailLogins) = splitAt ir logins
  let login = head tailLogins
  let remainLogins = tail tailLogins
  randLogins <- genRandLogins (headLogins ++ remainLogins)
  return ([login] ++ randLogins)

isLogin :: String -> Bool
isLogin login = if length login == 8 then checkLogin login else False
  where checkLogin ('x':xs) = (checkNum $ xs !! 5) && (checkNum $ xs !! 6) && all checkChars (take 5 $ xs)
        checkNum n = n `elem` ['0'..'9'] || n `elem` ['a'..'z']
        checkChars c = c `elem` ['a'..'z']

data AssList k d
  = Item k d (AssList k d)
  | EmptyItem
  deriving(Eq, Show)

test :: (Eq k) => AssList k d -> Bool
test (EmptyItem) = True
test (Item key _ itemList) = isUnique key itemList && test itemList

isUnique :: (Eq k) => k -> AssList k d -> Bool
isUnique key (EmptyItem) = True
isUnique key (Item nkey _ itemList) = key /= nkey && isUnique key itemList

fdup :: String -> IO ()
fdup filename = do
  h <- openFile filename ReadMode
  c <- hGetContents h
  let input = lines c
  let out = parseOnput input
  putStr out
  hClose h

parseOnput :: [String] -> String
parseOnput [] = ""
parseOnput (('+':'+':line):lines) = line ++ "\n" ++ line ++ "\n" ++ parseOnput lines
parseOnput (line:lines) = line ++ "\n" ++ parseOnput lines