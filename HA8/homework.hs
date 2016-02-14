-- HA8
import Data.Bits
import Data.List
import Prelude hiding (Left, Right)

-- 1
-- Basic functions needed to control a turtle.

-- x and y coordinates
type Position = (Integer, Integer)
data Orientation = Left | Right | Up | Down deriving (Eq, Show)
-- Clockwise and Counterclockwise
data TurnDir = CW | CCW deriving (Eq, Show)

-- 1a
data Turtle = Turtle {
  pos :: Position,
  orient :: Orientation,
  turnDir :: Maybe TurnDir} deriving Show
  
t1 = Turtle (5,5) Up (Just CW)

-- 1b
-- Returns the position of a turtle.
position :: Turtle -> Position
position t = pos t

-- 1c
-- Returns the orientation of a turtle.
orientation :: Turtle -> Orientation
orientation t = orient t

-- 1d
-- Defines a new turtle.
newTurtle :: Turtle
newTurtle = Turtle { pos = (0,0), orient = Up, turnDir = Nothing }

-- 1e
-- Moves the turtle a given amount in the direction it is currently facing.
move :: Integer -> Turtle -> Turtle
move n t@(Turtle pos orient _)
  | n < 0 = error "Turtles cannot move backwards"
  | orient == Left = t { pos = (x-n, y) }
  | orient == Right = t { pos = (x+n, y) }
  | orient == Up = t { pos = (x, y+n) }
  | otherwise = t { pos = (x, y-n) }
  where (x,y) = position t

-- 1f
-- Turns orientation clockwise.
turnCW :: Orientation -> Orientation
turnCW o
  | o == Left = Up
  | o == Right = Down
  | o == Up = Right
  | otherwise = Left

-- Turns orientation counter-clockwise.
turnCCW :: Orientation -> Orientation
turnCCW = turnCW . turnCW . turnCW 

-- Changes the turtle's orientation accordingly. 
turn :: TurnDir -> Turtle -> Turtle
turn tDir t = t { orient = newOrient }
  where o = orientation t
        newOrient = if tDir == CW then turnCW o else turnCCW o

-- 1g
-- Enables to chain commands to turtle more easily.
runTurtle :: [Turtle -> Turtle] -> Turtle -> Turtle
runTurtle xs t = foldl (\acc curr -> curr acc) t xs

-- 2
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show, Ord)
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

-- 2a
-- Removes those subtrees that do not satisft the given predicate.
treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter f Leaf = Leaf
treeFilter f n@(Node x Leaf Leaf) = if f x then n else Leaf
treeFilter f (Node x l r)
  | f x = Node x (treeFilter f l) (treeFilter f r)
  | otherwise = Leaf

-- 2b
-- Applies a binary function to the tree with the depth as the first argument.
levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap = levelMap' 0

levelMap' :: Int -> (Int -> a -> b) -> Tree a -> Tree b
levelMap' _ _ Leaf = Leaf
levelMap' d f (Node x l r) = Node (f d x) (levelMap' (d+1) f l) (levelMap' (d+1) f r)

-- 2c
-- Checks whether the first tree appears as a part of the second.
isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf _ = True
isSubtree _ Leaf = False
isSubtree sub@(Node x l r) t@(Node a b c)
  | compareTrees sub t = True
  | otherwise = (isSubtree sub b) || (isSubtree sub c)

-- Determines whether two trees are identical.
compareTrees :: Eq a => Tree a -> Tree a -> Bool
compareTrees Leaf Leaf = True
compareTrees Leaf _ = False
compareTrees _ Leaf = False  
compareTrees (Node x l r) (Node a b c) = (x == a) && (compareTrees l b) && (compareTrees r c)

-- 3
data Date = Date {
  day :: Int,
  month :: Int,
  year :: Int } deriving (Eq, Show)
  
-- 3a
-- Number of days in a month.
days = [0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- Checks if a year is leap.
isLeap :: Int -> Bool
isLeap year = (year `mod` 400 == 0) || ((year `mod` 4 == 0) && (year `mod` 100 /= 0))

-- Checks if date is valid.
isValid :: Int -> Int -> Int -> Bool
isValid dd mm yy
 | dd < 1 || mm < 1 || dd > 31 || mm > 12 = False
 | dd > (days !! mm) = False
 | dd == 29 && mm == 2 && (not $ isLeap yy) = False
 | otherwise = True

-- A constructor for Date that checks if it's valid.
date :: Int -> Int -> Int -> Maybe Date
date dd mm yy
 | isValid dd mm yy = Just $ Date dd mm yy
 | otherwise = Nothing

-- 3b
-- Adds one day to current date.
addOneDay :: Date -> Date
addOneDay d@(Date dd mm yy) = Date newDay newMonth newYear
  where maxDay = days !! mm
        overflow = (dd + 1) > maxDay || (dd == 28 && (not $ isLeap yy))
        newDay = if overflow then 1 else dd + 1
        newMonth' = if overflow then mm + 1 else mm
        newMonth = if newMonth' == 13 then 1 else newMonth'
        newYear = if newMonth' == 13 then yy + 1 else yy

-- Removes one day from current date.
removeOneDay :: Date -> Date
removeOneDay d@(Date dd mm yy) = Date newDay newMonth newYear
  where overflow = (dd - 1) < 1
        newMonth' = if overflow then mm - 1 else mm
        newMonth = if newMonth' == 0 then 12 else newMonth'
        newDay' = if overflow then (days !! (newMonth)) else dd - 1
        newDay = if newDay' == 29 && (not $ isLeap yy) && newMonth == 2 then 28 else newDay'
        newYear = if newMonth' == 0 then yy - 1 else yy

-- Adds n days to current date.
addDays' :: Date -> Int -> Date
addDays' d 0 = d
addDays' d n = addDays' (addOneDay d) (n-1)

-- Removes n days to current date.
removeDays' :: Date -> Int -> Date
removeDays' d 0 = d
removeDays' d n = removeDays' (removeOneDay d) (n+1)

-- Takes current date and n as a number of days to add and calculates the new date.
addDays :: Date -> Int -> Date
addDays d n
 | n <= 0 = removeDays' d n
 | otherwise = addDays' d n
 
-- 4
data Pred = And Pred Pred | Or Pred Pred | Not Pred | Val Bool deriving (Eq, Show)
expr = And (Or (Val True) (Not (Val True))) (Not (And (Val True) (Val False)))

-- Evaluates a boolean expression using a recursive data structure.
eval :: Pred -> Bool
eval (Val t) = t
eval (And t1 t2) = eval t1 && eval t2
eval (Or t1 t2) = eval t1 || eval t2
eval (Not t) = not $ eval t

-- 6a
-- Converts a binary number to Gray code.
toGrayCode :: (Integral a, Bits a) => a -> a
toGrayCode n = (shiftR n 1) `xor` n

-- 6b
-- Converts Gray code to binary.
fromGrayCode :: (Integral a, Bits a) => a -> a
fromGrayCode n = fromGrayCode' n (shiftR n 1)

-- Helper function that takes a number and a mask and converts Gray code to binary.
fromGrayCode' :: (Integral a, Bits a) => a -> a -> a
fromGrayCode' n 0 = n
fromGrayCode' n mask = fromGrayCode' (n `xor` mask) (shiftR mask 1)

-- 7
class Truthy a where
  truey :: a -> Bool
  falsey :: a -> Bool
  truey a = not $ falsey a
  falsey a = not $ truey a

instance Truthy Bool where
  truey b = b

instance Truthy Int where
  falsey 0 = True
  falsey n = False
  
instance Truthy [a] where
  falsey [] = True
  falsey xs = False
  
-- 7b
-- A function that behaves like if-then-else.
if' :: Truthy p => p -> a -> a -> a
if' pred yes no = if truey pred then yes else no

-- 7c
-- If the first argument evaluates to truey, it returns the second argument, otherwise it raises an error.
assert :: Truthy p => p -> a -> a
assert pred arg = if truey pred then arg else error "Assertion failed"

-- 7d
-- Defines an operator that behaves like && but on Truthy instances of Bool.
(&&&) :: (Truthy a, Truthy b) => a -> b -> Bool
a &&& b = truey a && truey b

-- Defines an operator that behaves like || but on Truthy instances of Bool.
(|||) :: (Truthy a, Truthy b) => a -> b -> Bool
a ||| b = truey a || truey b
