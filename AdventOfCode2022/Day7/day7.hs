import Data.List.Split
import Data.List
import System.IO
import Control.Monad
import Data.Maybe
import Data.Tuple.Select
import Data.Char

type ParentDir = String
type CurrentDir = String
type Size = Int
type Name = String

type Directory = (ParentDir, CurrentDir, Size)
type File = (ParentDir, Name, Size)

type Computer = ([Directory], [File])

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = filter (\x -> length x > 2) $ splitByNewLine contents
  let noLs = filter (\x -> x /= "$ ls") content
  print noLs
  -- let computer = ([],[]) :: Computer -- get part1 dirs
  let computer = readData noLs -- get part 1 solution
  print computer
  print $ foldl (\x y -> if sel3 y <= 100000 then x+(sel3 y) else x) 0 $ fst computer

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

readData :: [String] -> Computer
readData input = readData' input "/" ([("","/",0)],[])

readData' :: [String] -> CurrentDir ->  Computer -> Computer
readData' [] _ computer = computer
readData' (x:xs) currDir computer
  | isInfixOf "$ cd" x = readData' xs updatedDir computer --x is move to some dir
  | take 3 x == "dir" = readData' xs currDir updatedComputerDir -- x is dir sth - create dir
  | otherwise = readData' xs currDir updatedComputerFile -- x is createFile createFile, updateDir sizes
  where
    updatedDir
      | isInfixOf ".." x = parentDir -- find directory in list and get its parent
      | otherwise = drop 5 x -- remove "$ cd "
    parentDir = sel1 $ fromJust $ find (\(_,y,_) -> y == currDir) $ fst computer
    updatedComputerDir = (fst computer ++ [(currDir, drop 4 x, 0)], snd computer)
    updatedComputerFile = (updateSizes (fst computer) currDir size, snd computer ++ [(currDir,name,size)]) --undefined
    size = convertStringToInt $ takeWhile isDigit x
    name = tail $ dropWhile isDigit x

updateSizes :: [Directory] -> CurrentDir -> Size -> [Directory]
updateSizes directories currDir size = directories -- add some recursion here to sum up file size

convertStringToInt :: String -> Int
convertStringToInt = read
