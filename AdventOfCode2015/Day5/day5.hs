import Data.List.Split
import Data.List
import System.IO
import Control.Monad
import Data.Tuple.Select
import Data.Maybe

newtype Vowel = Vowel Char deriving(Show)

vowel :: Char -> Maybe (Vowel)
vowel x | x `elem` "aeiouAEIOU" = Just (Vowel x)
        | otherwise = Nothing

main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let lines = filter (\x -> length x > 0 ) $ splitByNewLine contents
  print $ sum $ map fromEnum $ map checkIfGood lines
  print $ sum $ map fromEnum $ map checkIfGoodPt2 lines

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

checkIfGood :: String -> Bool
checkIfGood input = containsThreeVowels input && containsTwiceInARow input && doesNotContainBlacklist input

checkIfGoodPt2 :: String -> Bool
checkIfGoodPt2 input = findIfContainsPalindromes input && findIfContainsCombos input

containsTwiceInARow :: String -> Bool
containsTwiceInARow input = greaterThanZero $ length $ filter (\x -> length x > 1) $ group input

greaterThanZero :: Int -> Bool
greaterThanZero input = input > 0

containsThreeVowels :: String -> Bool
containsThreeVowels input = (length $ filter (\x -> not $ isNothing x) $ map vowel input) >= 3

doesNotContainBlacklist :: String ->  Bool
doesNotContainBlacklist input
  | isInfixOf "xy" input = False
  | isInfixOf "pq" input = False
  | isInfixOf "ab" input = False
  | isInfixOf "cd" input = False
  | otherwise = True

allSubs :: Int -> String -> [String] -- for substrings
allSubs n s
    | length s >= n = take n s : allSubs n (tail s)
    | otherwise = []

findIfContainsPalindromes :: String -> Bool
findIfContainsPalindromes input = (length $ filter (\x -> x > 1) $ map length $ group $ sort ((filter filterPalindrome $ allSubs 3 input) ++ (filter filterPalindrome $ map reverse $ allSubs 3 input))) > 0
  where
    filterPalindrome = \x -> x !! 0 == x !! 2

findIfContainsCombos :: String -> Bool
findIfContainsCombos input = (length $ filter (\x -> x > 1) $ map length $ group $ sort $ map (\x -> x !! 0) $ group $ allSubs 2 input) > 0
