import Data.List

-- 1.1.
product' :: Num a => [a] -> a
product' [] = 1
product' [x] = x
product' (x:xs) = x * product' xs

-- 1.2.
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xs) = headsOf xs
headsOf (x:xs) = (head x) : (headsOf xs)

-- 2.1.
modMult n m [x] = [x * (n `mod` m)]
modMult n m (x:xs) = x * (n `mod` m) : modMult n m xs

-- 2.2.
addPredecessor :: Num a => [a] -> [a]
addPredecessor l = addPredecessor' l 0
  where addPredecessor' [] _ = []
        addPredecessor' (x:xs) prev = (x+prev) : (addPredecessor' xs x)
		
-- 3.1.
equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets [] = []
equalTriplets ((a,b,c):xs)
  | a == b && b == c = (a,b,c) : equalTriplets xs
  | otherwise = equalTriplets xs
  
-- 3.2.
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : (replicate' (n-1) x)

-- 4.1.
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs

drop'' :: Int -> [a] -> [a]
drop'' n xs
  | n < 0 = reverse $ drop' (-n) (reverse xs)
  | otherwise = drop' n xs

-- 4.2.
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo a b xs = temp2
  where temp = drop'' a xs
        cnt = length xs - b - 1
        temp2 = drop'' (-cnt) temp

-- 5.1.
eachThird [] = []
eachThird [x] = []
eachThird [x,y] = []
eachThird (x:y:z:xs) = z : eachThird xs

-- 5.2.
crossZip :: [a] -> [a] -> [(a,a)]
crossZip [] _ = []
crossZip _ [] = []
crossZip (x:xs) (y:ys) = (x,y) : crossZip xs ys

-- 6.1.
length' :: [a] -> Int
length' l = len l 0
  where len [] n = n 
        len (x:xs) n = len (xs) (n+1)
