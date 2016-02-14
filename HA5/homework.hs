-- HA5
import Data.List
import Data.Char

-- 1.
-- Implement Data.List.Intercalate using recursion.
intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs yss = concat $ intercalate'' xs (init yss) ++ [last yss]

intercalate'' :: [a] -> [[a]] -> [[a]]
intercalate'' _ [] = []
intercalate'' xs (ys:yss) = (ys ++ xs) : intercalate'' xs yss

-- 2.
type Probability = Double
type DiscreteRandVar = [(Int, Probability)]
x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]

-- 2 a
-- Calculate the expected value of a discrete random variable.
mean :: DiscreteRandVar -> Double
mean [] = 0
mean ((a,b):xs) = (fromIntegral a*b) + mean xs

mean' :: DiscreteRandVar -> Double
mean' xs = acc' xs 0
  where acc' [] n = n
        acc' ((a,b):xs) n = acc' xs (n + fromIntegral a*b)
		
-- 2 b
-- Calculate the variance of a discrete random variable.
variance :: DiscreteRandVar -> Double
variance xs = calculate xs m
  where m = mean xs
  
calculate :: DiscreteRandVar -> Double -> Double
calculate [] _ = 0
calculate ((a,b):xs) m = (fromIntegral a - m)^2 * b + calculate xs m

variance' :: DiscreteRandVar -> Double
variance' xs = calculate' xs m 0
  where m = mean xs

calculate' :: DiscreteRandVar -> Double -> Double -> Double
calculate' [] _ n = n
calculate' ((a,b):xs) m n = calculate' xs m (n + (fromIntegral a - m)^2 * b)

-- 2 c
-- Return a list of values with equal to or greater probability than given parameter.
probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter _ [] = []
probabilityFilter p ((a,b):xs)
  | b >= p = a : probabilityFilter p xs
  | otherwise = probabilityFilter p xs

probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' p xs = help p xs []

help :: Probability -> DiscreteRandVar -> [Int] -> [Int]
help _ [] l = l
help p ((a,b):xs) l
  | b >= p = help p xs (l ++ [a])
  | otherwise = help p xs l

-- 3 a
-- Split a list into a sublists of length n. If it's not divisible, the last sublist is shorter than n.
chunk :: Int -> [a] -> [[a]]
chunk 0 _ = []
chunk _ [] = []
chunk n s = (take n s) : chunk n (drop n s)

-- 3 b
-- Split a list into sublists of length given in another list. If the lengths do not add up, the remaining part is left unchunked.
chunkBy :: [Int] -> [a] -> [[a]]
chunkBy [] s = []
chunkBy _ [] = []
chunkBy (x:xs) s
  | x == 0 = chunkBy xs s
  | otherwise = (take x s) : chunkBy xs (drop x s)

-- 3 c
-- Split a list into n sublists of equal length. If not divisible, chunk the remainder into last sublist.
chunkInto :: Int -> [a] -> [[a]]
chunkInto 0 s = []
chunkInto _ [] = [[]]
chunkInto n s
  | len == 0 = chunkInto (n-1) s
  | otherwise = (take len s) : chunkInto (n-1) (drop len s)
  where len = length s `div` n

-- 4.
-- Evaluate an expression given in Reverse Polish notation.
rpnCalc :: String -> Int
rpnCalc "" = 0
rpnCalc s
  | not $ check s = error "Invalid RPN expression"
  | otherwise = last $ last $ rpnCalc' s []
  
check :: String -> Bool
check s = n1 + n1 + 1 == length s
  where n1 = length [x | x <- s, x `elem` ['^', '*', '/', '+', '-']]

rpnCalc' :: String -> [Int] -> [[Int]]
rpnCalc' [] l = []
rpnCalc' (x:xs) l = curr : (rpnCalc' xs curr)
  where curr = push l x
        push (x:y:xs) '^' = (y ^ x):xs
        push (x:y:xs) '*' = (x * y):xs
        push (x:y:xs) '/' = (y `div` x):xs
        push (x:y:xs) '+' = (x + y):xs
        push (x:y:xs) '-' = (y - x):xs
        push xs num = (digitToInt num):xs

-- 5 a
-- Implement Euclidean algorithm.
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

-- 5 b
-- Calculate GCD of integers in a list.
gcdAll :: [Int] -> Int
gcdAll [] = error "Cannot compute gcd of an empty list"
gcdAll xs = minimum $ gcdAll' xs

gcdAll' :: [Int] -> [Int]
gcdAll' [] = []
gcdAll' [x] = [x]
gcdAll' (x:y:xs) = (gcd' x y) : gcdAll' xs

-- 5 c
-- Implement extended Euclidean algorithm.
extendedGcd :: Int -> Int -> (Int, Int, Int)
extendedGcd a 0 = (a `div` abs a, 0, a)
extendedGcd a b = let (x, y, g) = extendedGcd b (a `mod` b)
                  in (y, x - (a `div` b) * y, g)

-- 6
-- Check whether a graph is bipartite.
type AdjacencyList = [Int]
type Graph = [AdjacencyList]

isBipartite :: Graph -> Bool
isBipartite xs = length unique == length (nub unique)
  where unique = concat $ nub xs

-- 7
-- Find all permutations of a list.
permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [y:zs | (y,ys) <- choose xs, zs <- permutations' ys]
  where choose [] = []
        choose (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- choose xs]
