import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import qualified Data.Text.Lazy as T
import Data.Time.Clock.POSIX
import System.Directory
import System.Environment
import System.IO
import System.IO.Error hiding (catch)

-- 1
time :: IO a -> IO Double
time f = do
  begin <- realToFrac <$> getPOSIXTime
  _ <- f
  end <- realToFrac <$> getPOSIXTime
  return $ end - begin
  
-- 2 a
grep :: String -> FilePath -> IO ()
grep s path = do
  f <- readFile path
  let sol = findGrep s f
  mapM_ putStrLn sol

findGrep :: String -> String -> [String]
findGrep sub = filter (isInfixOf sub) . lines

-- 2 b
grepWithArgs :: IO ()
grepWithArgs = do
  args <- getArgs
  f <- readFile (last args)
  let sol = findGrep (head args) f
  mapM_ putStrLn sol

-- 2 c
grepText :: T.Text -> FilePath -> IO ()
grepText s path = do
  f <- readFile path
  let sol = findGrepText s (T.pack f)
  mapM_ print sol
  
findGrepText :: T.Text -> T.Text -> [T.Text]
findGrepText sub = filter (T.isInfixOf sub) . T.lines

-- 3
type Table = (FilePath, [String])
type TableName = String

-- 3 a
dbCreateTable :: TableName -> [String] -> IO Table
dbCreateTable tName colLabels = do
  writeFile (tName ++ ".tbl") (intercalate " " colLabels)
  return ((tName ++ ".tbl"), [intercalate " " colLabels])

-- 3 b
dbDeleteTable :: Table -> IO ()
dbDeleteTable (f, colLabels) = removeFile f `catch` handleExists
  where handleExists e
         | isDoesNotExistError e = return ()
         | otherwise = throwIO e

-- 3 c
dbInsert :: Table -> [String] -> IO ()
dbInsert (f, cols) xs = do
  if (length cols == length xs) then appendFile f $ unlines [unwords xs]
                                else error "Can't insert row into table!"

-- 3 d
dbSelect :: Table -> ([String] -> Bool) -> IO [[String]]
dbSelect (f, cols) p = withFile f ReadMode $ \h -> do
  xs <- map words <$> lines <$> hGetContents h
  length xs `seq` return $ filter p xs

-- 3 e
dbDelete :: Table -> ([String] -> Bool) -> IO ()
dbDelete t@(f, cols) p = do
  accepted <- dbSelect t p
  writeFile (f ++ "temp") (unlines $ map unwords accepted)
  renameFile (f ++ "temp") f
  
-- 3 f
dbUpdate :: Table -> ([String] -> Bool) -> ([String] -> [String]) -> IO ()
dbUpdate t@(f, cols) p fun = do
  accepted <- dbSelect t p
  let sol = map fun accepted
  writeFile (f ++ "temp") (unlines $ map unwords sol)
  renameFile (f ++ "temp") f

-- 3 g
dbPrintTable :: Table -> IO ()
dbPrintTable table = do
  s <- dbSelect table (const True)
  putStr $ unlines $ map unwords s
