import Data.List.Split
import Data.List
import System.IO
import Control.Monad

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  putStrLn $ show $ part1 contents
  putStrLn $ show $ part2 contents

part1 :: String -> Int
part1 input = length(groups !! 0) - length( groups !! 1)
  where
    groups = group $ sort $ init input


part2 :: String -> Int
part2 input = part2rek input 0 0

part2rek :: String -> Int -> Int -> Int
part2rek [] _ _ = 0
part2rek (el:rest) id score
  | score == -1 = id
  | otherwise = part2rek rest (id+1) (score + (if el == '(' then 1 else -1))
