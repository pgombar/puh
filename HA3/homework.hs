-- Haskell HA3

import Data.Char
import Data.List
import System.Environment
import CSVUtils

-- 1
-- A function that interleaves elements of two lists by sequentially alternating between their elements.
interleave :: [a] -> [a] -> [a]
interleave l1 l2 = concat [[x] ++ [y] | (x, y) <- zip l1 l2]

-- 2
-- A function that extracts a slice from a list, given two indices i and j (inclusive). The order of the indices isn't important.
slice :: Int -> Int -> [a] -> [a]
slice i j l
  | i < 0 || j < 0 = error "Slice index out of range"
  | i >= length l || j >= length l = error "Slice index out of range"
  | otherwise = [l !! x | x <- (if j < i then [j..i] else [i..j])]
  
-- 3
-- A function that converts an identifier from a lower or upper camel case format to a regular one. In example, "wordCount" or "WordCount" should result in "word count".
decamel :: String -> String
decamel "" = error "identifier is empty"
decamel s
  | length s /= length (removeWhitespace s) = error "input not in camel case format"
  | otherwise = unwords [toLower (head x) : (tail x) | x <- l]
  where l = splitIt s 

-- A helper function that generates a list of indices of elements that are uppercase characters in a string. Later used to split string according to these indices.
getSplitIndexes :: String -> [Int]
getSplitIndexes s = 0 : [i | i <- [1 .. length s-1], isUpper $ s !! i] ++ [length s]

-- A helper function that splits a string according to a given list of indices.
splitIt :: String -> [[Char]]
splitIt s = [slice (l !! i) ((l !! (i+1))-1) s | i <- [0..length l-2]]
  where l = getSplitIndexes s

-- A helper function that removes whitespace from a string, used to check whether a string is in a camelcase format or not.
removeWhitespace :: String -> String  
removeWhitespace s = [x | x <- s, x /= ' ']

-- 4 a
-- A function that takes a list of elements and a single element and returns the number of occurrences of the element in the list.
count :: Eq a => [a] -> a -> Int
count s c = length [x | x <- s, x == c]

-- 4 b
-- A function that takes a list and returns a list where elements occurring only once have been filtered out.
removeUniques :: Eq a => [a] -> [a]
removeUniques l = [x | x <- l, count l x /= 1]

-- 5
-- A function that takes a string and a binary mask and preserves a character in the string only when a '1' is present in the mask.
type Mask = String
mask :: String -> Mask -> String
longmask = "1000000010000010000000001010000"
mask _ "" = ""
mask s m = [x | (x,y) <- zip s (cycle m), y == '1']

-- 6
-- A function that takes a list of people and their locations and find the name of the closest one to a given point.
type Point = (Int, Int)
type Friend = (Point, String)

findFriend :: Point -> [Friend] -> String
findFriend (px,py) [] = error "Nobody exists to be your friend"
findFriend (px,py) l = head [name | (point,name) <- l, distance point (px,py) == min]
  where min = findMinDistance (px,py) l

-- A helper function that finds the minimum distance in a list of friends and their locations according to a given location.
findMinDistance :: Point -> [Friend] -> Float
findMinDistance (px,py) l = minimum [distance point (px,py) | (point,name) <- l]

-- A helper function that returns the distance between two points.
distance :: Point -> Point -> Float
distance (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x2-x1)^2 + (y2-y1)^2

-- 7 a
-- A function that returns the multiplication table for numbers from 1 to n, n >= 1.
mulTable :: Int -> [[Int]]
mulTable n
  | n < 1 = error "Given number lesser than 1"
  | otherwise = [getTableRow x n | x <- [1..n]]

-- A helper function that returns one row of the multiplication table, starting with the number start.
getTableRow :: Int -> Int -> [Int]
getTableRow start end = [start * i | i <- [1..end]]

-- 7 b
-- A function that converts a given element to a string and left-pads it with spaces up to a given length.
leftpad :: Show a => Int -> a -> String
leftpad n int
  | n < 0 = error "Cannot pad to negative length"
  | length (show int) > n = error $ show int ++ " does not fit into " ++ show n ++ " character" ++ (if n /= 1 then "s" else "")
  | otherwise = take x (cycle " ") ++ show int
  where x = n - length (show int)

-- 7 c
-- A function that pretty-prints a list of lists in the form of a correctly-aligned table.
prettyTable :: Show a => [[a]] -> IO ()
prettyTable xss = do
  putStrLn $ init $ unlines [writeRow xss i | i <- [0..length xss-1]]

-- A helper function that returns a string that is the pretty-print representation of the i-th row of the table.
writeRow :: Show a => [[a]] -> Int -> String
writeRow xss i = unwords [leftpad n x | x <- xss !! i]
  where n = findLongest xss

-- A helper function that finds the longest number in a list of numbers.
findLongest :: Show a => [[a]] -> Int
findLongest xss = maximum [longestNumber l | l <- xss]

-- A helper function that returns the length of the longest number from the list represented in string form.
longestNumber :: Show a => [a] -> Int
longestNumber l = maximum [length (show x) | x <- l]

-- 8
-- in separate module CSVUtils
  
-- 9 a
-- A function that implements the wordcount utility, printing the number of lines, words and characters within a file.
wc :: FilePath -> IO ()
wc path = do
  f <- readFile path
  let lines1 = length [x | x <- lines f]
  let words1 = sum [length (words x) | x <- lines f]
  let chars1 = length f
  putStrLn $ show lines1 ++ " " ++ show words1 ++ " " ++ show chars1

-- 9 b
-- A function that implements the paste utility, pairing up lines from two input files and printing them joined with a tab character.
paste :: FilePath -> FilePath -> IO ()
paste file1 file2 = do
  f1 <- readFile file1
  f2 <- readFile file2
  let x = concat [a ++ "\t" ++ b ++ "\n" | (a,b) <- zip (lines f1) (lines f2) ]
  putStrLn $ init x

-- 9 c
-- A function that implements the cut utility, taking a delimiter, index and a file, then cuts out a portion of each line and writes them out on stdout.
cut :: String -> Int -> FilePath -> IO ()
cut delimiter col document = do
  f <- readFile document
  putStrLn $ workIt delimiter col f

-- A helper function that cuts the file according to the delimiter.
workIt :: Separator -> Int -> Document -> String
workIt delimiter col document
  | (col-1) < 0 || (col-1) >= length (head x) = error "Column index out of bounds"
  | otherwise = init $ unlines [e !! (col-1) | e <- x]
  where x = parse delimiter document
  
-- A helper function that parses a CSV document.
parse :: Separator -> Document -> [[String]]
parse delimiter document = [words $ replaceWithWhitespace (head delimiter) x | x <- lines document]

-- A helper function that replaces a delimiter charater with whitespace so we can call the 'words' function on the string.
replaceWithWhitespace :: Char -> String -> [Char]
replaceWithWhitespace delimiter line = [if x == delimiter then ' ' else x | x <- line]

