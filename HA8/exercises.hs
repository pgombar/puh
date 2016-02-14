import Data.List
import Control.Monad

-- Lecture 8

-- 1.1.
data Date = Date Int Int Int deriving Show

showDate :: Date -> String
showDate (Date d m y) = show d ++ "." ++ show m ++ "." ++ show y

-- 1.2.
data Point = Point Double Double deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point vx vy) (Circle2 (Point x y) r) = Circle2 (Point (x + vx) (y + vy)) r
translate (Point vx vy) (Rectangle2 (Point x1 y1) (Point x2 y2)) = Rectangle2 (Point (x1 + vx) (y1 + vy)) (Point (x2 + vx) (y2 + vx))

-- 1.3.
inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point x y) r) (Point a b) = (a-x)^2 + (b-y)^2 <= r^2
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point a b) = a >= x1 && a <= x2 && b >= y1 && b <= y2

inShapes :: [Shape2] -> Point -> Bool
inShapes xs (Point a b) = or $ map (\x -> inShape x (Point a b)) xs

-- 1.4.
data Vehicle = Car String Double | Truck String Double | Motorcycle String Double | Bicycle

hp :: Vehicle -> Double
hp (Car _ x) = x
hp (Truck _ x) = x
hp (Motorcycle _ x) = x
hp (Bicycle) = 0.2

totalHorsepower :: [Vehicle] -> Double
totalHorsepower = sum . map hp

-- 2
data Level = Bachelor | Master | PhD deriving (Show,Eq)

data Student = Student {
  firstName :: String,
  lastName :: String,
  studentId :: String,
  level :: Level,
  avgGrade :: Double } deriving Show

newStudent2 = Student "Pero" "Peric" "29397823" Master 3.0
newStudent3 = Student "Ana" "Anic" "81273897" Bachelor 2.5
newStudent4 = Student "Iva" "Ivic" "8728723" Bachelor 4.6

stud = [newStudent2,newStudent3,newStudent4]
  
-- 2.1.
improveStudent :: Student -> Student
improveStudent s@(Student _ _ _ _ g) = s { avgGrade = min (g+1) 5.0 }

-- 2.2.
avgGradePerLevel :: [Student] -> Level -> Double
avgGradePerLevel xs l = sum $ map (avgGrade) fil
  where fil = filter ((==l) . level) xs

avgGradePerLevels :: [Student] -> (Double, Double, Double)
avgGradePerLevels xs = (avgGradePerLevel xs Bachelor, avgGradePerLevel xs Master, avgGradePerLevel xs PhD)

-- 2.3.
rankedStudents :: Level -> [Student] -> [String]
rankedStudents l xs = map (studentId) sorted
  where fil = filter ((==l) . level) xs
        sorted = reverse $ sortBy (\x y -> compare (avgGrade x) (avgGrade y)) fil

-- 2.4.
addStudent :: Student -> [Student] -> [Student]
addStudent s xs
  | null (filter (\x -> studentId x == studentId s) xs) = s : xs
  | otherwise = error "Student already exists!"

-- 3
data MyTriplet a b c = MyTriplet(a,b,c) deriving Show

data Employee = Employee {
  name   :: String,
  salary :: Maybe Double } deriving Show

-- 3.1.
toTriplet :: MyTriplet a b c -> (a,b,c)
toTriplet (MyTriplet x) = x

-- 3.2.
toDouble :: Maybe Double -> Double
toDouble (Just n) = n
toDouble _ = 0

totalSalaries :: [Employee] -> Double
totalSalaries = foldl (\acc x -> toDouble (salary x) + acc) 0

-- 3.3.
addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 s xs
  | null (filter (\x -> studentId x == studentId s) xs) = Just $ s : xs
  | otherwise = Nothing

addStudent3 :: Student -> [Student] -> Either String [Student]
addStudent3 s xs
  | null (filter (\x -> studentId x == studentId s) xs) = Right (s : xs)
  | otherwise = Left "Student already exists!"

  
-- Lecture 9

-- 1.1.
-- what?

data Sex = Male | Female deriving (Show,Read,Eq,Ord)

data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Read,Eq,Ord)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane, paula]
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
paula  = Person2 "42" "Paula"  "Doe" Female (Just ann) Nothing Nothing []

-- 1.2.
motherCheck :: Person2 -> [String]
motherCheck p = case mother2 p of
  Just x -> map (personId2) $ children2 x
  Nothing -> []
  
fatherCheck :: Person2 -> [String]
fatherCheck p = case father2 p of
  Just x -> map (personId2) $ children2 x
  Nothing -> []

parentCheck :: Person2 -> Bool
parentCheck p = elem id (fatherCheck p) || elem id (motherCheck p)
  where id = personId2 p

-- 1.3.
getFemaleChildren :: Maybe Person2 -> [Person2]
getFemaleChildren p = case p of 
  Just x -> filter ((==Female) . sex2) $ children2 x
  Nothing -> []

getSiblings :: Person2 -> [Person2]
getSiblings p = getFemaleChildren (mother2 p) ++ getFemaleChildren (father2 p)

sister :: Person2 -> Maybe Person2
sister p
  | null fil = Nothing
  | otherwise = Just $ head fil
  where siblings = getSiblings p
        fil = filter ((/= personId2 p) . personId2) siblings

-- 1.4.
descendant :: Person2 -> [Person2]
descendant p = children2 p

-- 2
data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord)
l1 = 1 `Cons` Empty
l2 = 1 `Cons` (2 `Cons` (3 `Cons` Empty))

-- 2.1.
listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (Cons x _) = Just $ x

-- 2.2.
listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty = Empty
listMap f (Cons x xs) = (f x) `Cons` (listMap f xs)

-- 3.1.
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show)

intTree = Node 1 (Node 8 Null Null) (Node 3 Null Null)

treeMax :: Ord a => Tree a -> a
treeMax (Node y Null Null) = y
treeMax (Node y left right) = max y (max (treeMax left) (treeMax right))

-- 3.2.
treeToList :: Ord a => Tree a -> [a]
treeToList Null = []
treeToList (Node y Null Null) = [y]
treeToList (Node y left right) = (treeToList left) ++ [y] ++ (treeToList right)

-- 3.3.
levelCut :: Int -> Tree a -> Tree a
levelCut 0 _ = Null
levelCut 1 (Node x l r) = Node x Null Null
levelCut n (Node x l r) = Node x (levelCut (n-1) l) (levelCut (n-1) r)

-- 4.1.
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
  | x < y     = Node y (treeInsert x l) r
  | x > y     = Node y l (treeInsert x r)
  | otherwise = t

listToTree :: Ord a => [a] -> Tree a
listToTree xs = listToTree' xs Null

listToTree' [] t = t
listToTree' (x:xs) t = listToTree' xs (treeInsert x t)

-- 4.2.
sortAndNub :: Ord a => [a] -> [a]
sortAndNub = treeToList . listToTree
  
-- 5.1.
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show,Enum)

instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = False
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

-- 5.2.
instance Show Person2 where
  show p = forename2 p ++ " " ++ surname2 p

-- 6.1.
instance Eq a => Eq (MyList a) where
  l1 == l2 = listHead l1 == listHead l2

-- 6.2.
instance (Eq a, Ord a) => Eq (Tree a) where
  t1 == t2 = sortAndNub (treeToList t1) == sortAndNub (treeToList t2)
