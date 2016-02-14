-- HA2
-- Paula Gombar, 0036474619
import Data.Char
import Data.List
import System.Environment

-- 1 a)
toTitleCase s = unwords [capitalize e | e <- words s]
capitalize (x:xs) = toUpper x : xs

-- 1 b)
toTitleCase' "" _ = ""
toTitleCase' s l = unwords(capFirstWord (words s) : (capitalizeWords' s l))
capFirstWord (x:xs) = capitalize x
capitalizeWords' s l = tail [if notElem e l then capitalize e else e | e <- words s]

-- 2
trimN l n
  | 2*n > length l = l
  | otherwise = drop n $ take (n + length l - 2*n) l
  
-- 3
main = echoFile
echoFile = do
  [path] <- getArgs
  f <- readFile path
  putStrLn(unlines [toUpperCaseWord e | e <- lines f])

toUpperCaseWord x = [toUpper e | e <- x]

-- 4
onlyDivisible s n
  | n <= 0 = error "n must be positive!"
  | otherwise = [fst e | e <- zip s [0..], snd e `mod` n == 0]
  
-- 5
isATriangle t1 t2 t3 = ((fst t2 - fst t1) / (snd t2 - snd t1)) /= ((fst t3 - fst t1)/(snd t3 - snd t1))

triangleCounter l = length [(l !! x, l !! y, l !! z) | x <- [0..length l-1], y <- [1..length l-1], z <- [2..length l-1], isATriangle (l !! x) (l !! y) (l !! z) && x < y && y < z]

-- 6
reverseWords s = unwords $ reverse $ words s

-- 7
intersect' [] _ = []
intersect' _ [] = []
intersect' l1 l2 = [x | x <- l1, elem x l2]
difference l1 l2 = [x | x <- l1, notElem x l2]

-- 8a
isWellFormed m = and [length (head m) == length e && length e /= 0 | e <- m]

-- 8b
size m
  | not $ isWellFormed m = error "Matrix is malformed"
  | otherwise = (length m, length $ head m)

-- 8c
getElement m x y
  | x >= fst (size m) || y >= snd (size m) || x < 0 || y < 0 = error "Index out of bounds"
  | otherwise = m !! x !! y

-- 8d
getRow m i
  | i >= fst (size m) || i < 0 = error "Index out of bounds"
  | otherwise = m !! i

-- 8e
getCol m i
  | i >= snd (size m) || i < 0 = error "Index out of bounds"
  | otherwise = [e !! i | e <- m]
  
-- 8f
addMatrices m1 m2
  | not (isWellFormed m1) || not (isWellFormed m2) = error "Matrix is malformed"
  | not (size m1 == size m2) = error "Matrices are not of equal size"
  | otherwise = [zipWith (+) (m1 !! x) (m2 !! x) | x <- [0 .. length(m1)-1]]
  
-- 8g
transpose' m
  | not $ isWellFormed m = error "Matrix is malformed"
  | otherwise = [getCol m i | i <- [0 .. snd (size m) - 1]]
  
-- 8h
multMatrices m1 m2
  | not (isWellFormed m1) || not (isWellFormed m2) = error "Matrix is malformed"
  | snd (size m1) /= fst (size m2) = error "Incompatible matrix dimensions"
  | otherwise = [listElement i m2 | i <- m1]

listElement l1 m2 = [sumRowAndCol l1 (getCol m2 i) | i <- [0 .. snd (size m2)-1]]

sumRowAndCol l1 l2 = sum [(fst e) * (snd e) | e <- zip l1 l2]