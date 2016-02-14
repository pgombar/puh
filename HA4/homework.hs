import Data.List

-- 1.
-- A function that calculates the left factorial of a number (sum of all factorials between 0 and n-1).
leftFactorial :: Integer -> Integer
leftFactorial n = sum [fact e | e <- [0..n-1]]

-- A helper function that calculates n!
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact(n-1)

-- 2.
-- A function that computes the number of zeroes n! ends with.
factorialZeroes :: Int -> Int
factorialZeroes n = foldl (\x y -> x + countFive y 0) 0 [5, 10..n]
--factorialZeroes n = foldr1 (+) [countFive e 0 | e <- [1..n]]

-- A function that counts the number of 5s in a number.
countFive :: Int -> Int -> Int
countFive n i
  | n `mod` 5 == 0 = countFive (n `div` 5) (i+1)
  | otherwise = i
  
-- 3.
-- A function that, given a list in format [L1,L2,L3,R1,R2,R3], returns a list formatted as [L1,R1,L2,R2,L3,R3].
interleave :: [a] -> [a]
interleave l = if n `mod` 2 == 1 then sol ++ [l !! (split-1)] else sol
  where n = length l
        split = n `div` 2 + n `mod` 2
        l1 = take split l
        l2 = reverse $ take (split - n `mod` 2) $ reverse l
        sol = concat [[x,y] | (x, y) <- zip l1 l2]
		
-- 4.
-- A function that, for a given list containing no duplicates, returns a list of pairs (x,y) such that x /= y and contans no symmetric pairs (y,x).
pairs :: Eq a => [a] -> [(a, a)]
pairs l = concat [pairOne x | x <- tmp]
  where tmp = init $ tails l

-- A function that pairs up every element from the list with the first element.
pairOne :: [a] -> [(a, a)]
pairOne l = [(head l, b) | b <- tail l]

-- 5.
-- A function that finds the shortest repeating sublist of elements within a given list such that repeating the sublist a certain number of times, yields the original list.
shortestSub :: Eq a => [a] -> [a]
shortestSub [] = []
shortestSub s = head [x | x <- i, s == repeatUntil s x]
  where i = tail $ inits s
        n = length s

-- A function that repeats a sublist of a list until their lengths are the same.
repeatUntil :: [a] -> [a] -> [a]
repeatUntil s sub = concat $ replicate x sub
  where n = length s
        m = length sub
        x = n `div` m
		
-- 6
type Timestamp = [Int]

-- a)
-- A function that checks if a timestamp contains valid values.
isValidTimestamp :: Timestamp -> Bool
isValidTimestamp [] = False
isValidTimestamp [x] = x `elem` [0..59]
isValidTimestamp [x,y] = x `elem` [0..59] && y `elem` [0..59]
isValidTimestamp [x,y,z] = x `elem` [0..23] && y `elem` [0..59] && z `elem` [0..59]
isValidTimestamp _ = False

-- b)
-- A function that converts a given timestamp to seconds.
timestampToSec :: Timestamp -> Int
timestampToSec t
  | not $ isValidTimestamp t = error "Invalid timestamp"
  | otherwise = check t
  
-- A function that converts a timestamp to seconds.
check [x] = x
check [x,y] = x * 60 + y
check [x,y,z] = x * 3600 + y * 60 + z

-- c)
-- A function that calculates a temporal difference in seconds between two timestamps.
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff t1 t2
  | not $ isValidTimestamp t1 && isValidTimestamp t2 = error "Invalid timestamp"
  | otherwise = abs $ x - y
  where x = timestampToSec t1
        y = timestampToSec t2  

-- 7 a
-- A function that, given a list of elements, returns a list of pairs of (element, number of occurrences of the element).
counts :: Ord a => [a] -> [(a, Int)]
counts s = [(head a, length a) | a <- tmp]
  where tmp = group $ sort s

-- 7 b
-- A function that that, given a list in format [L1,L2,L3,R1,R2,R3], returns a list formatted as [L1,R1,L2,R2,L3,R3]
group' :: Eq a => [a] -> [[a]]
group' s = [take b (repeat a) | (a, b) <- count s]

-- A function that returns a list of pairs of (element, number of occurrences of the element). 
count :: Eq a => [a] -> [(a, Int)]
count l = [(a, countIt a l) | a <- nub l]

-- A function that counts the number of occurrences of element x in a list.
countIt :: Eq a => a -> [a] -> Int
countIt x l = length [e | e <- l, e == x]

-- 7 c
-- A function that, given a list of elements, returns a list of pairs of (element, number of occurrences of the element). The difference between 7a is that it does not have a type constraint Ord.
counts' :: Eq a => [a] -> [(a, Int)]
counts' xs = [(head x, length x) | x <- group' xs]

-- 8
type Grid = [String]
-- 8 a
-- A function that computes the minimal number of moves to complete a simplified version of Lights Out!
lightsOutLite :: Grid -> Int
lightsOutLite m
  | not $ checkValid m = error "Broken grid!"
  | otherwise = sum [sum [1 | x <- row, x == '1'] | row <- m]

-- A function that checks whether a grid is valid.
checkValid :: Grid -> Bool
checkValid m = length (nub [length x | x <- m]) == 1

-- 8b
-- ???

-- 9 a
-- A function that, given a String, returns all possible strings that are one edit distance away from it.
oneEdits :: String -> [String]
oneEdits s = sort $ nub $ foo1 ++ foo2 ++ foo3 ++ foo4
  where foo1 = extra s
        foo2 = oneLess s
        foo3 = replaceIt s
        foo4 = swapChars s

-- A function that returns all possible strings with an extra inserted character.
extra :: String -> [String]
extra s = concat [extraChar s c | c <- ['a'..'z']]

-- A function that inserts character c on all possible positions.
extraChar :: String -> Char -> [String]
extraChar s c = [a ++ [c] ++ b | i <- [0..length s], let (a,b) = splitAt i s]

-- A function that returns all possible strings with one less character.
oneLess :: String -> [String]
oneLess s = [a ++ tail b | i <- [0..length s-1], let (a,b) = splitAt i s]

-- A function that returns all possible strings with one of its characters replaced with another one.
replaceIt :: String -> [String]
replaceIt s = concat [replaceChar s c | c <- ['a'..'z']]

-- A function that replaces every character with character c.
replaceChar :: String -> Char -> [String]
replaceChar s c = [a ++ [c] ++ tail b | i <- [0..length s-1], let (a,b) = splitAt i s]

-- A function that swaps two neighbouring characters.
swapChars :: String -> [String]
swapChars s = [init a ++ [head b] ++ [last a] ++ tail b | i <- [1..length s-1], let (a,b) = splitAt i s]

-- 9 b
-- A function that, given a String, returns all possible strings that are two edit distances away from it. 
twoEdits :: String -> [String]
twoEdits s = sort $ nub $ concat [oneEdits x | x <- (oneEdits s)]

-- testing functions
compareToFile :: (String -> [String]) -> String -> FilePath -> IO Bool
compareToFile f s file = do
  list <- readFile file
  return $ f s == (read list :: [String])
  
testOneEdits :: IO Bool
testOneEdits = compareToFile oneEdits "hi" "oneEdits.txt"

testTwoEdits :: IO Bool
testTwoEdits = compareToFile twoEdits "hi" "twoEdits.txt"
-- /testing functions

-- 10
type Seed = Int
-- 10 a
-- A function that generates a pseudorandom integer given a seed value using the linear congruential generator algorithm and the parameters listed below.
a = 1664525
c = 1013904223
m = 2^32
fromSeed :: Seed -> Int
fromSeed x = (a * x + c) `mod` m

-- 10 b
-- A function that, given a starting seed value and number limit, asks the player to guess a number between 0 and the limit, inclusively. The number is generated pseudorandomly. In case the player’s guess is too low, the function should return LT. If it is too high, it should return GT. Finally, if the player is really lucky and manages to guess the number, it should return EQ.
guess :: Seed -> Int -> IO Ordering
guess seed limit = do
  n <- readLn
  return $ (compare n random)
  where random = fromSeed seed `mod` (limit + 1)
