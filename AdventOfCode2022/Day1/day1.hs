import Data.List.Split
import Data.List
import System.IO
import Control.Monad

main = do
  let input = []
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let lines = splitByNewLine contents
  putStrLn $ show $ part1 lines
  putStrLn $ show $ part2 lines

part1 :: [String] -> Int
part1 = sum . take 1 . reverse . sort . mainf

part2 :: [String] -> Int
part2 = sum . take 3 . reverse . sort . mainf

mainf :: [String] -> [Int]
mainf input = calculateElfsCallories $ splitByEmptyLine input

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

splitByEmptyLine :: [String] -> [[String]]
splitByEmptyLine = splitOn [""]

calculateElfsCallories :: [[String]] -> [Int]
calculateElfsCallories = map calculateElfCallories

calculateElfCallories :: [String] -> Int
calculateElfCallories l = sumCallories $ map convertStringToInt l

convertStringToInt :: String -> Int
convertStringToInt = read

sumCallories :: [Int] -> Int
sumCallories = sum
