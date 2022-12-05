import Data.List.Split
import Data.List
import System.IO
import Control.Monad

main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let inputs = filter (\x -> length x > 1) $ splitByNewLine contents
  print inputs
  print $ sum $ map calculatePaper inputs
  print $ sum $ map calculateRibbon inputs

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

calculatePaper :: String ->  Int
calculatePaper input = cost
  where
    splitByX = splitOn "x" input
    dimensions = map convertStringToInt splitByX
    cost = sortedDimensions !! 0 * sortedDimensions !! 1  + 2*(dimensions !! 0 * dimensions !! 1 + dimensions !! 1 * dimensions !! 2 + dimensions !! 2 * dimensions !! 0)
    sortedDimensions = sort dimensions

calculateRibbon :: String ->  Int
calculateRibbon input = cost
  where
    splitByX = splitOn "x" input
    dimensions = map convertStringToInt splitByX
    cost = 2 * (sortedDimensions !! 0 + sortedDimensions !! 1) + (dimensions !! 0 * dimensions !! 1 * dimensions !! 2)
    sortedDimensions = sort dimensions

convertStringToInt :: String -> Int
convertStringToInt = read
