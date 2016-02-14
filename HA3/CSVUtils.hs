module CSVUtils
( parseCSV
, showCSV
, colFields
, readCSV
, writeCSV
, Separator
, Document
, doc
, brokenDoc
) where

import Data.List

-- 8
-- Define necessary types and strings.
type Separator = String
type Document = String
type CSV = [Entry]
type Entry = [Field]
type Field = String
doc = "John;Doe;15\nTom;Sawyer;12\nAnnie;Blake;20"
brokenDoc = "One;Two\nThree;Four;Five"

-- 8 a
-- A function that takes a separator and a string representing a CSV document and returns a CSV representation of the document.
parseCSV :: Separator -> Document -> CSV
parseCSV delimiter document
  | not $ doesItAppear delimiter document = error $ "The character '" ++ delimiter ++ "' does not occur in the text"
  | not $ isWellFormed delimiter document = error "The CSV file is not well-formed"
  | otherwise = parse delimiter document
  
-- A helper function that parses a CSV document.
parse :: Separator -> Document -> [[String]]
parse delimiter document = [words $ replaceWithWhitespace (head delimiter) x | x <- lines document]

-- A helper function that replaces a delimiter charater with whitespace so we can call the 'words' function on the string.
replaceWithWhitespace :: Char -> String -> [Char]
replaceWithWhitespace delimiter line = [if x == delimiter then ' ' else x | x <- line]

-- A helper function that returns true if a character appears in a list of strings, otherwise false.
doesItAppear :: String -> String -> Bool
doesItAppear c document = length [x | x <- lines document, isInfixOf c document] > 0

-- A helper function that returns true if a document is well-formed (contains an equal number of fields per row).
isWellFormed :: Separator -> Document -> Bool
isWellFormed delimiter document = length (nub [length x | x <- xss]) == 1
  where xss = parse delimiter document

-- 8 b
-- A function that takes a separator and a CSV representation of a document and creates a CSV string from it.
showCSV :: Separator -> CSV -> Document
csv = parseCSV ";" doc
showCSV delimiter document
  | length (nub [length x | x <- document]) /= 1 = error "The CSV file is not well-formed"
  | otherwise = init sol
  where sol = unparse delimiter document

-- A helper function that reverses the process of parsing a CSV document.
unparse :: Separator -> CSV -> String
unparse delimiter document = unlines [replaceWithDelimiter (head delimiter) x | x <- document]

-- A helper function that replaces whitespaces with a delimiter.
replaceWithDelimiter :: Char -> [String] -> String
replaceWithDelimiter delimiter line = [if x == ' ' then delimiter else x | x <- unwords line]

-- 8 c
-- A function that takes a CSV document and a field number and returns a list of fields in that column.
colFields :: Int -> CSV -> [Field]
colFields i document
  | length (nub [length x | x <- document]) /= 1 = error "The CSV file is not well-formed"
  | i >= length (head document) = error $ "There is no column " ++ show i ++ " in the CSV document"
  | otherwise = [x !! i | x <- document]

-- 8 d
-- A function that takes a file path and a separator and returns the CSV representation of the file.
readCSV :: Separator -> FilePath -> IO CSV
readCSV delimiter filePath = do
  f <- readFile filePath
  let x = parseCSV delimiter f
  return x

-- 8 e
-- A function that takes a separator, a file path and a CSV document and writes the document into a file.
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV delimiter destination document = do
  let x = showCSV delimiter document
  writeFile destination x
  