import Data.List hiding (insert)
import Data.Char
import Data.Ord

-- 1a
-- Function that takes list elements as long as the predicate evaluates to True.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' cond xs = helper cond xs []
  where helper cond [] temp = temp
        helper cond (x:xs) temp
         | cond x = helper cond xs (temp ++ [x])
         | otherwise = temp

-- 1b
-- Function that drops list elements as long as the predicate evaluates to True.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' cond [] = []
dropWhile' cond l@(x:xs)
  | cond x = dropWhile' cond xs
  | otherwise = l
  
-- 1c
-- Function that combines two lists using a given function.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

-- 2
-- An efficient sortBy that calculates the expression used for comparison only once per term.
efficientSortBy f xs = map fst $ sortBy (comparing snd) efficient
  where efficient = map (\x -> (x, f x)) xs

-- 3a
-- Function that stems a word. It discards the suffix up to the last vowel (inclusive), but only if the word with the discarded suffix is at least as long as the suffix itself.
stemmer1 :: String -> String
stemmer1 s
  | canBeDiscarded s suffL = s
  | otherwise = fst $ splitAt suffPos s
  where suffPos = getSuffix s
        suffL = length s - suffPos

-- Helper function that checks whether a Char is a vowel.
isVowel :: Char -> Bool
isVowel s = s `elem` ['a', 'e', 'i', 'o', 'u']

-- Helper function that checks whether a suffix can be discarded.
canBeDiscarded :: String -> Int -> Bool
canBeDiscarded s suffL = length s - suffL < suffL

-- Helper function that returns the position of the suffix beginning.
getSuffix :: String -> Int
getSuffix s = last [i | (i,c) <- zip [0..] s, isVowel c]

-- 3b
suffixes = ["ing", "s", "es", "er"]
-- Function that discards a suffix if it is provided in a list of suffixes, being the longest one. If no suffix can be discarded or the word doesn't contain any of them, the word remains unchanged.
stemmer2 :: [String] -> String -> String
stemmer2 suffixes s = take (length s - length suffix) s
  where isSuffix suff s = elem suff (tails s)
        validSuffixes = [suff | suff <- suffixes, isSuffix suff s]
        sortedSuffixes = reverse $ map snd $ sort $ zip (map length validSuffixes) validSuffixes
        suffix = if null sortedSuffixes then [] else head sortedSuffixes

-- 3c
-- Function that works like stemmer2 but uses stemmer1 instead if none of the suffixes appear in the word.
stemmer3 :: [String] -> String -> String
stemmer3 suffixes s
  | sol == s = stemmer1 s
  | otherwise = sol
  where sol = stemmer2 suffixes s

-- 3d
pairs = [("driving", "driv"), ("fools", "fool"), ("teacher", "teach")]

-- Function that tests the precision of a stemmer. It takes a sample as a list of pairs (word, correctSuffix) and a stemming function.
testStemmer :: [(String, String)] -> (String -> String) -> Double
testStemmer pairs f = (fromIntegral x) / (fromIntegral y) * 100
  where stems = map (f . fst) pairs
        comparison = zip stems (map snd pairs)
        noCorrect = filter (\(x,y) -> x == y) comparison
        x = length noCorrect
        y = length pairs

-- 3e
-- Function that takes a stemmer function, a predicate and a string and returns a string consisting of stemmed words, but only if they satisfy the predicate.
stemText :: (String -> String) -> (String -> Bool) -> String -> String
stemText f predicate s = unwords $ map f filtered
  where filtered = filter predicate (words s)

-- 4a
type Point = (Double, Double)
type Centroid = Point
type Cluster = (Point, [Point])

-- Function that calculates the centroid of a list of 2D points.
centroid :: [Point] -> Point
centroid [] = error "Cannot calculate centroid of zero points!"
centroid points = (sumX / n, sumY / n)
  where allX = map fst points
        allY = map snd points
        n = fromIntegral $ length points
        sumX = foldl1 (+) allX
        sumY = foldl1 (+) allY

-- 4b
-- Function that takes a list of points and a list of centroids and forms clusters.
groupByDist :: [Point] -> [Point] -> [Cluster]
groupByDist _ [] = error "Cannot group around less than one point"
groupByDist xs ys = updateClusters xs ys initialClusters
  where initialClusters = constructInitialClusters ys

-- Function that constructs initial clusters from a list of centroids.
constructInitialClusters :: [Centroid] -> [Cluster]
constructInitialClusters centroids = map (\x -> (x, [])) centroids

-- Function that updates clusters, re-arranging points to the clusters with the closest centroid.
updateClusters :: [Point] -> [Centroid] -> [Cluster] -> [Cluster]
updateClusters [] centroids clusters = clusters
updateClusters vectors@(v:vs) centroids clusters = updateClusters vs centroids newClusters
  where closest = closestCentroid v centroids
        newClusters = addPointToCluster v closest clusters
  
-- Function that adds a point to a cluster based on the centroid.
addPointToCluster :: Point -> Centroid -> [Cluster] -> [Cluster]
addPointToCluster p centr cluster = map (updateCluster p centr) cluster
  where updateCluster point centr cluster@(c, xs) 
         | centr == c    = (c, point : xs)
         | otherwise     = cluster

-- Function that returns the distance between two points, squared.
getDist :: Point -> Point -> Double
getDist (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

-- Finds the closest centroid to a given point.
closestCentroid :: Point -> [Point] -> Point
closestCentroid point centroids = head closest
  where distances = map (\x -> getDist x point) centroids
        minDist = minimum distances
        closest = filter (\x -> getDist x point == minDist) centroids

-- 4c
-- Function that clusters the points into k clusters and i iterations at most.
cluster :: [Point] -> Int -> Int -> [Cluster]
cluster [] _ _ = error "Cannot cluster for no points"
cluster xs k i 
  | k > length xs = error "The number of groups cannot be greater than the number of elements"
  | i == 0 = groupByDist xs (chooseInitialCentroids xs k)
  | otherwise = iterativeKmeans i xs initialClusters
  where initialCentroids = chooseInitialCentroids xs k
        initialClusters  = constructInitialClusters initialCentroids

-- Function that re-clusters the clusters and re-calculates the centroids in i iterations.
iterativeKmeans :: Int -> [Point] -> [Cluster] -> [Cluster]
iterativeKmeans 0 _ clusters = clusters
iterativeKmeans iter points clusters
  | centroids == newCentroids = iterativeKmeans 0 points newClusters
  | otherwise                 = iterativeKmeans (iter-1) points newClusters 
  where centroids = map fst clusters
        newClustersTemp = updateClusters points centroids clusters
        newClusters = updateCentroids newClustersTemp
        newCentroids = map fst newClusters

-- Function that updates centroids based on the points in the cluster.
updateCentroids :: [Cluster] -> [Cluster]
updateCentroids clusters = map (\(a,b) -> (centroid b, b)) clusters

-- Function that chooses initial centroids, being the first k of the input.
chooseInitialCentroids :: [Point] -> Int -> [Centroid]
chooseInitialCentroids [] _ = []
chooseInitialCentroids _ 0 = []
chooseInitialCentroids (x:xs) k = x : chooseInitialCentroids xs (k-1)

-- 5a
-- Function that sorts the tracks by track number.
sortTracks :: [String] -> [String]
sortTracks tracks = map snd zipped
  where zipped = sort $ map zipIt tracks

-- Function that extracts the track number from the list of tracks.
zipIt :: String -> (Int, String)
zipIt s = (n, s)
  where elem = findNumber (words s)
        n = read elem

-- Function that recognizes the number in the string.
findNumber :: [String] -> String
findNumber xs = head $ filter (\x -> elem '0' x) xs

-- 5b
-- Function that calculates the number of played tracks for a whole album.
numberOfPlays :: [String] -> Integer
numberOfPlays xs = foldl1 (+) prep
  where prep = map (read . head . words) xs

-- 6
-- Function that determines whether the string consists of the words in the dictionary.
doYouSpeak :: [String] -> String -> Bool
doYouSpeak xs s = dropItLikeItsHot s pimped
  where pimped = map lowerCase $ filter (\x -> isInfixOf x s) xs

-- Function that flips a String to all lowercase letters.
lowerCase :: String -> String
lowerCase s = map toLower s

-- Function that drops one prefix at a time from a word.
dropItLikeItsHot [] _ = True
dropItLikeItsHot s [] = False
dropItLikeItsHot s (x:xs) = dropItLikeItsHot (drop (length x) s) xs

-- 7
-- Function that takes a string and returns an ASCII-formatted histogram of occurring letters.
histogram :: String -> IO ()
histogram s = mapM_ putStrLn sol
  where trans = transpose $ generateLists s
        l1 = "--------------------------"
        l2 = "abcdefghijklmnopqrstuvwxyz"
        sol = trans ++ [l1] ++ [l2]

-- Function that filters out non-letter characters and ignores the casing.
pimp :: String -> String
pimp = lowerCase . filter isLetter

-- Function that returns the number of occurrences of a Char in a String.
noOcc :: String -> Char -> Int
noOcc s c = length $ filter (==c) s

-- Function that maps the number of occurrences in a word for each letter of the alphabet.
getAlphabet :: String -> [(Char, Int)]
getAlphabet s = map (\x -> (x, noOcc s x)) ['a'..'z']

-- Function that generates lists for each character of the alphabet, consisting of '*' and ' ', depending on the number of occurrences of the character in the word.
generateLists :: String -> [String]
generateLists s = map (\(x, occ) -> generateOne occ maxOcc) occs
  where occs = getAlphabet s
        maxOcc = maximum $ map snd occs

-- Function that generates a list for a single character based on its number of occurrences.
generateOne :: Int -> Int -> String
generateOne occ maxOcc = (replicate noBlanks ' ') ++ (replicate occ '*')
  where noBlanks = maxOcc - occ

-- 8a
circle = [0.01, 0.02 .. 2*pi]

type Range = Double
-- Function that returns a smoothed version of a given function by averaging the sum of f(x-dx), f(x) and f(x+dx).
smooth :: Range -> (Double -> Double) -> Double -> Double
smooth dx f x = (f (x-dx) + f x + f (x+dx)) / 3.0

-- 8b
-- Function that applies a given function n times.
nfold :: Int -> (a -> a) -> a -> a
nfold n f = foldl1 (.) (replicate n f)

-- 8c
-- Function that smoothes a given function n times.
nsmooth :: Int -> Range -> (Double -> Double) -> Double -> Double
nsmooth n dx f = nfold n $ smooth dx f

-- 9
type WordCounter = [(String, Integer)]
testwc = [("dog", 3),("spelling", 2),("cool", 4)]

-- 9a
-- Function that checks whether a word is in a wordCounter.
contains :: WordCounter -> String -> Bool
contains wc word = word `elem` map fst wc

-- 9b
-- Function that inserts a word into a wordCounter. If it's already an element, the number of occurrences is incremented by 1, otherwise a new tuple is added (word, 1).
insert :: WordCounter -> String -> WordCounter
insert wc word
  | contains wc word = updateWc wc word
  | otherwise = wc ++ [(word, 1)]

-- Function that updates a wordCounter.
updateWc :: WordCounter -> String -> WordCounter
updateWc [] _ = []
updateWc ((w,n):wc) word
  | w == word = (w, n+1) : (updateWc wc word)
  | otherwise = (w, n) : (updateWc wc word)

-- 9c
-- Function that returns a number of occurrences of a word in a wordCounter.
get :: WordCounter -> String -> Integer
get wc word = getNumber l
  where l = filter (\(w,n) -> w == word) wc
        getNumber xs
          | null xs = 1
          | otherwise = 1 + snd (head xs)

-- 9d
-- Function that returns an empty spellchecker.
empty :: WordCounter
empty = []

-- 9e
-- Function that takes a list of words and returns a wordcounter containing the word counts of words within the list.
train :: [String] -> WordCounter
train words = train' words []

-- Function that takes a list of words, builds a wordcounter from it and returns it.
train' :: [String] -> WordCounter -> WordCounter
train' [] wc = wc
train' (x:xs) wc = train' xs newWc
  where newWc = insert wc x

-- 9f
-- Function that takes a FilePath and returns a wordCounter containing all the words contained within. All words are turned to lowercase and only words consisting exclusively of letters are included.
trainFromFile :: FilePath -> IO WordCounter
trainFromFile path = do
  file <- readFile path
  return $ (train $ extractWords file)

-- Function that filters only the words consisting of letters and turns it to lowercase.
extractWords :: String -> [String]
extractWords s = map lowerCase $ filter onlyLetters (words s)

-- Function that checks whether a word consists of only letters.
onlyLetters :: String -> Bool
onlyLetters s = length s == length clean
  where clean = filter isLetter s

-- 9g
-- Function that returns a likeliest word based on the given word.
likeliestWord :: WordCounter -> String -> String
likeliestWord wc word
  | not (onlyLetters word) = word
  | wc `contains` word = word
  | null filtered = word
  | otherwise = getMaxOcc filtered
  where filtered = filter (\(s,n) -> checkWord s word) wc

-- Function that checks whether a word can be considered as a correction of a given word.
checkWord :: String -> String -> Bool
checkWord wcWord word = (word1 == wcWord1) || (wcWord1 `elem` oneEd) || (wcWord1 `elem` twoEd)
  where word1 = lowerCase word
        wcWord1 = lowerCase wcWord
        oneEd = oneEdits word1
        twoEd = twoEdits word1

-- Function that returns the word with max occurrence in wordCounter.
getMaxOcc :: WordCounter -> String
getMaxOcc wc = fst $ head $ filter (\(s,n) -> n == maxOcc) wc
  where maxOcc = maximum $ map snd wc

-- 9h
-- Function that separates the punctuation from a word into other strings and returns a dissected list.
separatePunct :: String -> [String]
separatePunct s = [word] ++ (dissect punc)
  where i = findPunct s 0
        word = fst $ splitAt i s
        punc = snd $ splitAt i s

-- Function that dissects a string by separating the punctuation.
dissect :: String -> [String]
dissect [] = []
dissect (x:xs) = [x] : dissect xs
  
-- Function that returns the index of the beginning of punctuation in a word.
findPunct :: String -> Int -> Int
findPunct [] n = n
findPunct (x:xs) n
  | isPunctuation x = n
  | otherwise = findPunct xs (n+1)
  
-- 9i
-- Function that takes a FilePath to the file used for training and a String representing a sentence and returns the sentence with spellchecking performed.
spellchecker :: FilePath -> String -> IO String
spellchecker path s = do
  file <- readFile path
  return $ spellchecker' file s

-- Function that performs the spellchecking on a String and returns the spellchecked String. 
spellchecker' :: String -> String -> String
spellchecker' checker s = unwords $ preserve (words s) corrected
  where w = map (\x -> lowerCase (head (separatePunct x))) (words checker)
        training = train w
        cleanInput = map (\x -> lowerCase (head (separatePunct x))) (words s)
        corrected = map (likeliestWord training) cleanInput

-- Function that preserves punctuation in a list of words.
preserve :: [String] -> [String] -> [String]
preserve original corrected = map (\(x,y) -> preserveWord x y) (zip original corrected)

-- Function that preserves punctuation in a word, i.e. the word "dawg," is corrected and becomes "dog,".
preserveWord :: String -> String -> String
preserveWord original corrected = concat $ [corrected] ++ tail (separatePunct original)

-- A function that, given a String, returns all possible strings that are one edit distance away from it.
oneEdits :: String -> [String]
oneEdits s = sort $ nub $ foo1 ++ foo2 ++ foo3 ++ foo4
  where foo1 = extra s
        foo2 = oneLess s
        foo3 = replaceIt s
        foo4 = swapChars s

-- A function that returns all possible strings with an extra inserted character.
extra :: String -> [String]
extra s = concat [extraChar s c | c <- ['a'..'z']]

-- A function that inserts character c on all possible positions.
extraChar :: String -> Char -> [String]
extraChar s c = [a ++ [c] ++ b | i <- [0..length s], let (a,b) = splitAt i s]

-- A function that returns all possible strings with one less character.
oneLess :: String -> [String]
oneLess s = [a ++ tail b | i <- [0..length s-1], let (a,b) = splitAt i s]

-- A function that returns all possible strings with one of its characters replaced with another one.
replaceIt :: String -> [String]
replaceIt s = concat [replaceChar s c | c <- ['a'..'z']]

-- A function that replaces every character with character c.
replaceChar :: String -> Char -> [String]
replaceChar s c = [a ++ [c] ++ tail b | i <- [0..length s-1], let (a,b) = splitAt i s]

-- A function that swaps two neighbouring characters.
swapChars :: String -> [String]
swapChars s = [init a ++ [head b] ++ [last a] ++ tail b | i <- [1..length s-1], let (a,b) = splitAt i s]

-- A function that, given a String, returns all possible strings that are two edit distances away from it. 
twoEdits :: String -> [String]
twoEdits s = concat $ map oneEdits (oneEdits s)
