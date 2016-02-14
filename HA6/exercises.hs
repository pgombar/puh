import Data.List
import Data.Char

-- 1.1.
takeThree :: [a] -> [a]
takeThree = take 3

dropThree :: [a] -> [a]
dropThree = drop 3

hundredTimes :: a -> [a]
hundredTimes = replicate 100

-- 1.2.
index = zip [0..]
index' = (`zip` [0..])

-- 1.3.
divider = (`replicate` '=')

-- 2.1.
applyOnLast f xs ys = f (last xs) (last ys)

addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = applyOnLast addThree xs ys 100

-- 2.2.
applyManyTimes n f x
  | n <= 0 = x
  | otherwise = applyManyTimes (n-1) f (f x)
  
applyTwice f x = applyManyTimes 2 f x

-- 3.1.
listifyList = map (:[])

-- 3.2.
cutOff n = map (min n)

-- 4.1.
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (^2) $ filter even xs

-- 4.2.
freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (==x) xs

-- 4.3.
-- 4 "kikiriki" -> "iiii"
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (\x -> freq x xs >= n) xs

-- 5.1.
withinInterval n m xs = filter (\x -> x >= n && x <= m) xs

-- 5.2.
sndColumn :: [[a]] -> [a]
sndColumn = map (!! 1)

-- 5.3.
canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs xs = map (\(x,y) -> (min x y, max x y)) tmp
  where tmp = filter (\(x,y) -> x /= y) xs


