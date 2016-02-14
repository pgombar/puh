import Data.List
import Data.Char

-- in class exercises
-- 1.1.
remove l = tail $ take (length l - 3) l

-- 1.2.
initials s1 s2 = take 1 s1 ++ ". " ++ take 1 s2 ++ "."

-- 1.3.
con s1 s2 = if length s1 > length s2 then s1 ++ s2 else s2 ++ s1

-- 1.4.
safeHead l
  | l == [] = []
  | otherwise = [head l]
  
-- 1.5.
hasDuplicates l = length l /= length (nub l)

-- 2.1.
doublesFromTo a b = [x*2 | x <- (if b < a then [b..a] else [a..b])]

-- 2.2.
-- Redefine 'ceasarCode n' so that it shifts all letters a specified number of positions 'n', converts all input to lowercase, and ensures that letters remain within the ['a'..'z'] interval.
caesarCode n s = [chr(check $ shift n e) | e <- s]
shift n s = ord (toLower s) + n
check n
  | n > 122 = n `mod` 122 + 97 - 1
  | otherwise = n

-- 3.1.
letterCount s = length (trim s)
trim s = concat [e | e <- words s, e /= " " && length e >= 3]

-- 3.2.
isPalindrome s = low (removeWhitespace s) == low (reverse (removeWhitespace s))
low s = [toLower e | e <- s]
removeWhitespace s = [e | e <- s, e /= ' ']

-- 3.3. ["water", "is", "warm"] mrawsiretaw
flip' xss = concat (reverse [reverse e |  e <- xss])

-- 4.1.
-- redefine the function so it takes an additional argument, resolution of the grid
inCircle r x y = [(x1,y1) |  x1 <- [-10 .. 10], y1 <- [-10 .. 10], (x-x1)^2 + (y-y1)^2 < r^2]
inCircle' r x y x1 y1 = [(x1,y1) |  x1 <- [-10, -10+x1 .. 10], y1 <- [-10, -10+y1 .. 10], (x-x1)^2 + (y-y1)^2 < r^2]

-- 4.2.
steps xs = zip xs $ tail xs

-- 5.1.
indices x xs = [snd e | e <- zip xs [1..], fst e == x]

-- 5.2.
-- - Define 'showLineNumbers s' that prefixes all lines from string 's' with a line number.
-- showLineNumbers "first line\nsecond line" => "1 first line\n2 second line\n"
showLineNumbers s = unlines [show(fst x) ++ " " ++ snd x | x <- zip [1..] (lines s)]

-- 5.3.
-- - Define 'haveAlignment xs ys' that returns 'True' if 'xs' and 'ys' have any identical elements that are aligned (appear at the same position in both lists).
haveAlignment xs ys = length(pimp xs ys) > 0
pimp xs ys = [x | x <- zip xs [1..], y <- zip ys [1..], fst x == fst y && snd x == snd y]

-- 6.4.
-- Define 'common xs ys' that returns the aligned subsequences. haveAlignment "water" "fire" => True common "witer" "fire" => "ie"
common xs ys = [fst x | x <- pimp xs ys]
