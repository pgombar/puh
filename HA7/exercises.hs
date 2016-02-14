import Data.Char
import Data.List

-- 1
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..] 

filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

-- 2.1
maxDiff :: [Int] -> Int
maxDiff xs = maximum $ map (abs . uncurry (-)) tmp
  where tmp = zip xs (tail xs)

minDiff :: [Int] -> Int
minDiff xs = minimum $ map (abs . uncurry (-)) tmp
  where tmp = zip xs (tail xs)

-- 2.1.1.
maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (maxDiff xs, minDiff xs)

-- 2.2
studentsPassed :: [(String, Double)] -> [String]
studentsPassed xs = map fst $ filter (\(x,y) -> y >= m/2) xs
  where m = maximum $ map snd xs
  
-- 3.1.
isTitleCased :: String -> Bool
isTitleCased s = length (words s) == length tmp
  where tmp = filter (isUpper . head) $ words s
  
-- 4.1.
elem' y = foldr (\x a -> a || x == y) False

-- 4.2.
reverse' = foldr (\x a -> a ++ [x]) []

-- 5.1.
reverse'' :: [a] -> [a]
reverse'' = foldl (\a x -> x : a) []

-- 5.2.
sumEven' = foldl (\a (x,y) -> if even x then y+a else a) 0 . zip [0..]

