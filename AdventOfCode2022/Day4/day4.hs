import Data.List.Split
import System.IO

type Sections = (Int,Int)

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = filter (\x -> length x > 2) $ splitByNewLine contents
  let pairedSections = map (\x -> pairSections $ splitByComma x) content
  let part1Solutions = map part1 pairedSections
  let part2Solutions = map part2 pairedSections
  print pairedSections
  print part1Solutions
  print $ sum $ map fromEnum part1Solutions
  print part2Solutions
  print $ sum $ map fromEnum part2Solutions

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

pairSections :: [String] -> [Sections]
pairSections = map convertToSections

splitByComma :: String -> [String]
splitByComma = splitOn ","

convertToSections :: String -> Sections
convertToSections input = (read $ splitByDash !! 0, read $ splitByDash !! 1)
  where
    splitByDash = splitOn "-" input

part1 :: [Sections] -> Bool
part1 input = isSubset firstSection secondSection || isSubset secondSection firstSection
  where
    firstSection = input !! 0
    secondSection = input !! 1

part2 :: [Sections] -> Bool
part2 input = partiallyCovered firstSection secondSection || partiallyCovered secondSection firstSection
  where
    firstSection = input !! 0
    secondSection = input !! 1

isSubset :: Sections -> Sections -> Bool
isSubset a b
  | fst a <= fst b && snd b <= snd a = True
  | otherwise = False

partiallyCovered :: Sections -> Sections -> Bool
partiallyCovered a b = not $ notCovering a b

notCovering :: Sections -> Sections -> Bool
notCovering a b
  | snd a < fst b = True
  | fst a > snd b = True
  | otherwise = False
