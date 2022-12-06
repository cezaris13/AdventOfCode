import Data.List
import System.IO

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  print $ solution contents 4 -- part 1
  print $ solution contents 14 -- part 2

solution :: String -> Int -> Int
solution input windowSize = solution' str last4 windowSize
  where
    str = drop windowSize input
    last4 = take windowSize input

solution' :: String -> String -> Int -> Int
solution' [] _ id1 = id1
solution' (x:xs) last4 id
  | isStringFromUniqueChars last4 = id
  | otherwise = solution' xs (tail last4 ++ [x]) (id+1)

isStringFromUniqueChars :: String -> Bool
isStringFromUniqueChars input = length (nub input) == length input
