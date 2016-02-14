import Data.Char
import Data.List

-- 1.1.
headHunter :: [[a]] -> a
headHunter ([]:[]:[]:_) = error "No heads!"
headHunter ([]:[]:c:_) = head c
headHunter ([]:b:_) = head b
headHunter (a:_) = head a

-- 1.2.
firstColumn :: [[a]] -> [a]
firstColumn m = [head x | x <- m ]

-- 1.3.
shoutOutLoud :: String -> String
shoutOutLoud s = unwords [rep e | e <- words s]
rep :: [a] -> [a]
rep (x:xs) = x : x : x : xs

-- 2.1.
pad :: String -> String -> (String, String)
pad (a:s1) (b:s2) = (toUpper a : padIt (n - n1) s1, toUpper b : padIt (n - n2) s2)
  where n1 = length s1
        n2 = length s2
        n = max n1 n2
padIt :: Int -> String -> String
padIt n s = s ++ take n (repeat ' ')

-- 2.2.
median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
  | odd l     = realToFrac $ ys !! h
  | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
  where l  = length xs
        h  = l `div` 2
        ys = sort xs

quartiles :: [Int] -> (Double, Double, Double)
quartiles l = (median (take x l), median l, median (drop y l))
  where len = length l
        x = len `div` 2 - ( if ( len `mod` 2 == 1 ) then 0 else 1 )
        y = len `div` 2 + 1
-- 3
-- redo 2 using let instead of where
pad' :: String -> String -> (String, String)
pad' (a:s1) (b:s2) =
  let n1 = length s1
      n2 = length s2
      n = max n1 n2
	  in (toUpper a : padIt (n - n1) s1, toUpper b : padIt (n - n2) s2)
padIt' :: Int -> String -> String
padIt' n s = s ++ take n (repeat ' ')

median' :: (Integral a, Fractional b) => [a] -> b
median' [] = error "median: Empty list"
median' xs = let l  = length xs
                 h  = l `div` 2
                 ys = sort xs
		     in if(l `mod` 2 == 1) then realToFrac $ ys !! h else realToFrac (ys !! h + ys !! (h-1)) / 2

quartiles' :: [Int] -> (Double, Double, Double)
quartiles' l = 
  let len = length l
      x = len `div` 2 - (if (len `mod` 2 == 1) then 0 else 1)
      y = len `div` 2 + 1
  in (median (take x l), median l, median (drop y l))

-- 4.1.
fun :: (Int, Int) -> [Int] -> String
fun (a,b) (x:y:xs) = "The pair " ++ case (a,b) of
  (1,1) -> "contains two ones"
  (_,1) -> "contains one one"
  (1,_) -> "contains one one"
  (_,_) -> "does not contain a single one"
  ++ " and the second element of the list is " ++ show y