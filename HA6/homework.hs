-- HA6
import Data.Char
import Control.Monad

-- 1
-- A function that partitions a list using a user-provided predicate function.
partition :: [a -> Bool] -> [a] -> [[a]]
partition [] _ = []
partition (x:xs) ys = filter (\y -> x y) ys : partition xs ys

-- 2
-- A function that maps function from list xs to list ys.
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] _ = []
cycleMap xs ys = cycleMap' (cycle xs) ys
  where cycleMap' _ [] = []
        cycleMap' [] _ = []
        cycleMap' (x:xs) (y:ys) = x y : cycleMap xs ys
		
-- 3 a
-- A recursive function that reduces a list of elements to a single element using a seed value and a binary reduction function.
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce f start [] = start
reduce f start (x:xs) = reduce f sol xs
  where sol = f start x
  
-- 3 b
-- A function that behaves like reduce, but assumes the input list contains at least one element.
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 _ [] = error "reduce1 got an empty list"
reduce1 f (x:xs) = reduce f x xs

-- 3 c
-- A function that return a list of all the intermediate values with the result at the end.
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan f start xs = start : scan' f start xs
  where scan' f start [] = []
        scan' f start (x:xs) = (f start x) : scan' f (f start x) xs

-- 4 a
-- A variant of reduce that operates from right to left.
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce f start [] = start
rreduce f start (x:xs) = f x (rreduce f start xs)
  
-- 4 b
-- A variant of rreduce that assumes the input list contains at least one element.
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ [] = error "rreduce1 got an empty list"
rreduce1 f xs = rreduce f (last xs) (init xs)

-- 4 c
-- A variant of the scan function that operates from right to left.
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan f start xs = rscan' f start (reverse xs) ++ [start]
  where rscan' f start [] = []
        rscan' f start (x:xs) = rscan' f (f x start) xs ++ [f x start]

-- 5 a
-- A function that computes an approximation of the square root of a number using a special case of Newton's method.
type Tolerance = Double
newton :: Tolerance -> Double -> Double
newton tol n
  | n < 0 = error "Can't get sqrt of negative number"
  | otherwise = calc' tol n 1 1

calc' :: Tolerance -> Double -> Double -> Double -> Double
calc' tol n guess old
  | abs (old-tmp) < tol = tmp
  | otherwise = calc' tol n tmp guess
  where tmp = (guess + n/guess) / 2

-- 5 b
-- A function that computes the derivative of a given function.
deriv :: (Double -> Double) -> Double -> Double
dx = 0.00001
deriv f x = (f (x + dx) - f x) / dx

-- 6
-- A function that checks whether a number is happy number.
isHappy :: Int -> Bool
isHappy n = checkHappy n []

checkHappy :: Int -> [Int] -> Bool
checkHappy n l
  | sol == 1 = True
  | sol `elem` l = False
  | otherwise = checkHappy sol (sol : l)
  where sol = sumSqDigits n

sumSqDigits :: Int -> Int
sumSqDigits 0 = 0
sumSqDigits n = (n `mod` 10)^2 + sumSqDigits (n `div` 10)

