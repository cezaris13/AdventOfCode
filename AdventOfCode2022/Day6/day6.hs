import Data.List.Split
import Data.List
import System.IO
import Control.Monad
import Data.Tuple.Select

type VisitedHouses = [(X,Y,Count)]
type CurrCoordinate = (X,Y)
type X = Int
type Y = Int
type Count = Int

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let oneLine = splitByNewLine contents !! 0
  print $ part1 oneLine
  print $ part2 oneLine

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

part1 :: String -> Int
part1 input = part1' str last4 4
  where
    str = drop 4 input
    last4 = take 4 input

part1' :: String -> String -> Int -> Int
part1' [] _ id = id
part1' (x:xs) last4 id
  | isStringFromUniqueChars last4 = id
  | otherwise = part1' xs (tail last4 ++ [x]) (id+1)

isStringFromUniqueChars :: String -> Bool
isStringFromUniqueChars input = (length $ nub input) == (length input)

part2 :: String -> Int
part2 input = part1' str last14 14
  where
    str = drop 14 input
    last14 = take 14 input
