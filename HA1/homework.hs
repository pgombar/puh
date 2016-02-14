import Data.Char

-- HA problem 1
strlenInRange string left right = if left < 0 || right < 0 then foo else checker string left right

checker s a b = length s >= a && length s <= b
foo = error "String length cannot be a negative number"

-- HA problem 2
isHereAGreater l index value = if index >= length l then False else l !! index > value

-- HA problem 3
wordFilter = do
  sentence <- getLine
  taboo <- getLine
  let result = unwords [ e | e <- words sentence, e /= taboo ]
  putStrLn(result)
  
-- HA problem 4
ord3 a b c =
  if a < b then
    if a < c then
	  if b < c then a:b:c:[] else a:c:b:[]
    else c:a:b:[]
  else
    if b < c then
      if a < c then b:a:c:[] else b:c:a:[]
	else c:b:a:[]

-- HA problem 5
-- a)
norm t = sqrt (fromIntegral (fst t)^2 + fromIntegral (snd t)^2)
-- b)
add t1 t2 = (fst t1 + fst t2, snd t1 + snd t2)
-- c)
scalarMult t x = (fst t * x, snd t * x)
-- d)
dot t1 t2 = fst t1 * fst t2 + snd t1 * snd t2

-- HA problem 6
asciiRange c1 c2 = zip [c1 .. c2] [ord c1 .. ord c2]

-- HA problem 7
incn n s = [ chr(ord e + n) | e <- s ]