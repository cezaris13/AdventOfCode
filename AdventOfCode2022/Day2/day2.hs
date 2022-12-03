import Data.List.Split
import Data.List
import System.IO
import Control.Monad

data GameValues = Rock | Paper | Scissors deriving (Enum, Show, Eq)
data Outcomes = Lose | Draw | Win deriving (Enum, Show, Eq)

main = do
  let input = []
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let pt1Ans = part1 contents
  print $ show pt1Ans
  let pt2Ans = part2 contents
  print $ show pt2Ans

part1 :: String -> Int
part1 input = sum $ answers
  where
    lines = filter (\x -> length x > 2) $ splitByNewLine input
    lines1 = map lineToChars lines
    answers = map (\x -> part1solution $ lineToGameValues x) lines1

part2 :: String -> Int
part2 input = sum $ answers
  where
    lines = filter (\x -> length x > 2) $ splitByNewLine input
    lines1 = map lineToChars lines
    answers = map (\x -> part2solution $ lineToGameValueAndOutcome x) lines1

lineToChars :: String -> (Char,Char)
lineToChars input = (head $ concat $ take 1 splitData, head $ splitData !! 1)
  where splitData = splitOn " " input

lineToGameValues :: (Char,Char) -> (GameValues, GameValues)
lineToGameValues a = (charToGameValue $ fst a, charToGameValue $ snd a)

lineToGameValueAndOutcome :: (Char,Char) -> (GameValues, Outcomes)
lineToGameValueAndOutcome a = (charToGameValue $ fst a, charToOutcome $ snd a)

part1solution :: (GameValues, GameValues) -> Int
part1solution p1
  | fst p1 == Rock && snd p1 == Rock = 3 + fromEnum Rock + 1
  | fst p1 == Rock && snd p1 == Paper = 6 + fromEnum Paper + 1
  | fst p1 == Rock && snd p1 == Scissors = 0 + fromEnum Scissors + 1
  | fst p1 == Paper && snd p1 == Rock = 0 + fromEnum Rock + 1
  | fst p1 == Paper && snd p1 == Paper = 3 + fromEnum Paper + 1
  | fst p1 == Paper && snd p1 == Scissors = 6 + fromEnum Scissors + 1
  | fst p1 == Scissors && snd p1 == Rock = 6 + fromEnum Rock + 1
  | fst p1 == Scissors && snd p1 == Paper = 0 + fromEnum Paper + 1
  | fst p1 == Scissors && snd p1 == Scissors = 3 + fromEnum Scissors + 1

part2solution :: (GameValues, Outcomes) -> Int
part2solution p1
  | fst p1 == Rock && snd p1 == Lose = 0 + fromEnum Scissors + 1
  | fst p1 == Rock && snd p1 == Draw = 3 + fromEnum Rock + 1
  | fst p1 == Rock && snd p1 == Win = 6 + fromEnum Paper + 1
  | fst p1 == Paper && snd p1 == Lose = 0 + fromEnum Rock + 1
  | fst p1 == Paper && snd p1 == Draw = 3 + fromEnum Paper + 1
  | fst p1 == Paper && snd p1 == Win = 6 + fromEnum Scissors + 1
  | fst p1 == Scissors && snd p1 == Lose = 0 + fromEnum Paper + 1
  | fst p1 == Scissors && snd p1 == Draw = 3 + fromEnum Scissors + 1
  | fst p1 == Scissors && snd p1 == Win = 6 + fromEnum Rock + 1

charToGameValue :: Char -> GameValues
charToGameValue 'A' = Rock
charToGameValue 'B' = Paper
charToGameValue 'C' = Scissors
charToGameValue 'X' = Rock
charToGameValue 'Y' = Paper
charToGameValue 'Z' = Scissors

charToOutcome :: Char -> Outcomes
charToOutcome 'X' = Lose
charToOutcome 'Y' = Draw
charToOutcome 'Z' = Win

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

splitByEmptyLine :: [String] -> [[String]]
splitByEmptyLine = splitOn [""]
