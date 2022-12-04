import Data.List.Split
import Data.List
import System.IO
import Control.Monad

main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = filter (\x -> length x > 2) $ splitByNewLine contents
  let solution1 = sum $ map pt1 content
  let travelers = chunksOf 3 content
  let solution2 = sum $ map pt2 travelers
  print content
  print solution1
  print solution2

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

splitInHalf :: String -> (String,String)
splitInHalf a = splitAt (length a `div` 2) a

pt1 :: String -> Int
pt1 test = getCodes $ filterSameElement groupedLetters
  where
    halfOfString = splitInHalf test
    firstPart = getUniqueElements $ fst halfOfString
    secondPart = getUniqueElements $ snd halfOfString
    uniqItems = firstPart ++ secondPart
    groupedLetters = groupByLetters uniqItems

pt2 :: [String] -> Int
pt2 test = getCodes $ filterForBadges filteredElems
  where
    filteredElems = groupByLetters $ concat $ map getUniqueElements test

getUniqueElements :: String -> String
getUniqueElements a = nub a -- fix later

groupByLetters :: String -> [String]
groupByLetters letters = group $ sort letters

filterSameElement :: [String] -> Char
filterSameElement a = head $ filter (\x -> length x == 2) a !! 0

filterForBadges :: [String] -> Char
filterForBadges a = head $ filter (\x -> length x == 3) a !! 0

getCodes :: Char -> Int
getCodes input
  | fromEnum input < 97 = fromEnum input -38
  | otherwise = fromEnum input -96
