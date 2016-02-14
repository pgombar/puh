import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Set (Set, member)
import System.Directory
import System.Exit
import System.IO

-- 1.1.
main1 = do
  s1 <- getLine
  s2 <- getLine
  print $ reverse $ s1 ++ s2

-- 1.2.
threeNumbers = do
  a <- getLine
  b <- getLine
  c <- getLine
  print $ (read a + read b + read c)
  
main2 = threeNumbers

-- 2.1.
threeStrings :: IO Int
threeStrings = do
  a <- getLine
  b <- getLine
  c <- getLine
  let x = a ++ b ++ c
  print x
  return $ length x
  
-- 2.2.
askNumber9 :: IO Int
askNumber9 = do
  x <- getLine
  let y = filter (isNumber) x
  if length y /= length x then askNumber9 else return $ read x
  
main3 = askNumber9

-- 2.3.
askUser :: String -> (String -> Bool) -> IO String
askUser m p = action m p

action m p = do
  print m
  s <- getLine
  if p s then return s else action m p

-- 2.4.
inputStrings :: IO [String]
inputStrings = foo []

foo xs = do
  s <- getLine
  if null s then return xs else foo $ xs ++ [s]

-- 3.1.
foo2 = do
  n <- read <$> getLine
  xs <- sequence (replicate n getLine)
  putStr $ unlines $ reverse xs
  
-- 3.4.
triplets :: IO ()
triplets = do
  mapM_ print [(x, y, z) | x <- [1..100], y <- [1..100], z <- [1..100], x ^ 2 + y ^ 2 == z ^ 2]

-- 4.1.
filterOdd :: IO ()
filterOdd = interact (unlines . map (snd) . filter (odd . fst) . zip [1..] . lines)

-- 4.2.
numberLines :: IO ()
numberLines = interact (unlines . map (\(a,b) -> (show a) ++ " " ++ b) . zip [0..] . lines)

-- 4.3.
filterWords :: Set String -> IO ()
filterWords set = interact (unlines . map (unwords) . map (removeWords set) . map (words) . lines)

removeWords :: Set String -> [String] -> [String]
removeWords set s = filter (\x -> not $ member x set) s

-- 5.1.
wc :: FilePath -> IO (Int, Int, Int)
wc f = withFile f ReadMode $ \h -> do
  s <- hGetContents h
  let l = lines s
      w = sum $ map (length . words) l
      c = sum $ map (sum . map (length) . words) l
  length s `seq` return (c, w, length l)

-- 5.2.
copyLines :: [Int] -> FilePath -> FilePath -> IO ()
copyLines xs path1 path2 = do
  h1 <- openFile path1 ReadMode
  h2 <- openFile path2 WriteMode
  lines' <- lines <$> hGetContents h1
  lines <- lines <$> hGetContents h1
  let filtered = [x | (x, i) <- zip lines [0..], i `elem` xs]
  forM_ filtered $ \x -> hPutStrLn h2 x
  hClose h1
  hClose h2

-- 6.1.
wordTypes :: FilePath -> IO Int
wordTypes f = do
  s <- words <$> readFile f
  return $ length $ nub s
  
-- 6.2.
diff :: FilePath -> FilePath -> IO ()
diff f1 f2 = do
  s1 <- lines <$> readFile f1
  s2 <- lines <$> readFile f2
  let diff = filter (uncurry (/=)) $ zip s1 s2
  forM_ diff $ \x -> putStr $ unlines ["< " ++ fst x, "> " ++ snd x]

-- 6.3.
removeSpaces :: FilePath -> IO ()
removeSpaces f = do
  (fTemp, hTemp) <- openTempFile "" f
  s <- readFile f
  let s' = map (reverse . dropWhile (== ' ') . reverse) $ lines s
  hPutStr hTemp $ unlines s'
  hClose hTemp
  renameFile fTemp f
