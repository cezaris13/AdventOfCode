import Data.List.Split
import Data.List
import System.IO
import Data.Maybe
import Data.Tuple.Select
import Data.Char

type DirectoryL = String
type Size = Int
type Name = String

type Directory = (DirectoryL, Name, Size)
type File = (DirectoryL, Name, Size)

type Computer = ([Directory], [File])

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = filter (\x -> length x > 2) $ splitByNewLine contents
  let noLs = filter (\x -> not $ isInfixOf "$ ls" x && length x > 0) content
  let computer = readData noLs -- get part 1 solution
  print $ foldl (\x y -> if sel3 y <= 100000 then x+(sel3 y) else x) 0 $ fst computer
  let freeSpace = (sel3 $ fromJust $ find (\x-> (sel2 x) == "/") $ fst computer) - 40000000
  print $ fromJust $ find (\x-> x>=freeSpace) $ sort $ map (\(_,_,z) -> z) $ fst computer -- part 2

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

readData :: [String] -> Computer
readData input = readData' input "" ([("","/",0)],[])

readData' :: [String] -> DirectoryL ->  Computer -> Computer
readData' [] _ computer = computer
readData' (x:xs) currDir computer
  | isInfixOf "$ cd" x = readData' xs updatedDir computer
  | take 3 x == "dir" = readData' xs currDir updatedComputerDir
  | otherwise = readData' xs currDir addedFile
  where
    updatedDir = updateDir (drop 5 x) currDir
    updatedComputerDir = updateComputerDirectories computer currDir (drop 4 x) 0
    updatedComputerFile = updateComputerFiles computer currDir size -- add file here
    addedFile = (fst updatedComputerFile, (snd updatedComputerFile) ++ [(currDir,name,size)])
    size = convertStringToInt $ takeWhile isDigit x
    name = getFileName x

updateComputerFiles :: Computer -> DirectoryL -> Size -> Computer
updateComputerFiles computer "/" size = (map (\x -> if sel2 x == "/" then (sel1 x, sel2 x, (sel3 x)+size) else x ) $ fst computer,snd computer)
updateComputerFiles computer directory size = updateComputerFiles (map (\x -> if (goToDirectory (sel1 x) (sel2 x)) == directory then (sel1 x, sel2 x,(sel3 x)+size) else x) $ fst computer,snd computer) (goUpInDirectory directory) size

isDirectoryExists :: [Directory] -> String -> Maybe Directory
isDirectoryExists directories directory = find (\(x,y,_) -> (goToDirectory x y) == directory) $ directories

updateDir :: String -> DirectoryL -> String
updateDir str currDir
  | isInfixOf ".." str = goUpInDirectory currDir
  | otherwise = goToDirectory currDir str

goUpInDirectory :: String -> String
goUpInDirectory input
  | length removedDir == 1 = removedDir
  | otherwise = reverse $ drop 1 $ removedDir
  where
    removedDir = dropWhile (\x -> x /= '/' ) $ reverse input

goToDirectory :: String -> String -> String
goToDirectory input directory = input ++ (if length input == 0 ||(length input > 0 && last input == '/') then "" else "/") ++ directory

getCurrentDirectory :: String -> String
getCurrentDirectory input = reverse $ takeWhile (\x -> x /= '/') $ reverse input

getFileName :: String -> String
getFileName input = tail $ dropWhile isDigit input

convertStringToInt :: String -> Size
convertStringToInt = read

updateComputerDirectories :: Computer -> DirectoryL -> String -> Size -> Computer
updateComputerDirectories computer currDir dirName size = (fst computer ++ [(currDir, dirName, size)], snd computer)
