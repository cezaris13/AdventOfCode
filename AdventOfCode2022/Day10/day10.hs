import Data.List.Split
import Data.List
import System.IO

type Cycle = Int
type RegisterValue = Int
type Iteration = (Cycle,RegisterValue)
type History = [Iteration]

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = filter (\x -> length x > 0) $ splitByNewLine contents
  print $ foldl (\x (y,z) -> x+y*z) 0 $ filter (\(x,y) -> elem x [20,60,100,140,180,220]) $ executeCommands 1 [] content -- part 1
  putStrLn $ unlines $ map (\x -> drawLine x "") $ filter (\x -> length x > 1) $ chunksOf 40 $ map (\(x,y) -> (x `mod` 40,y)) $ executeCommands 1 [(1,1)] content -- part 2

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

executeCommands :: Cycle -> History -> [String] -> History
executeCommands _ hist [] = hist
executeCommands cycle hist (x:xs)
  | isInfixOf "noop" x = executeCommands (cycle+1) (repeatLastVal hist) xs
  | otherwise = executeCommands (cycle+2) updatedHist xs
  where
    updatedHist = hist ++ [(cycle+1,lastVal),newIterVal]
    newIterVal = (cycle+2, lastVal + (convertStringToInt $ drop 4 x))
    lastVal = if length hist == 0 then 1 else snd $ last hist

convertStringToInt :: String -> Int
convertStringToInt = read

repeatLastVal :: History -> History
repeatLastVal [] = []
repeatLastVal hist = hist ++ [(fst lastCommand+1,snd lastCommand)]
  where
    lastCommand = last hist

-- part 2

drawLine :: History -> String -> String
drawLine [] line = line
drawLine (x:xs) line = drawLine xs updatedLine
  where
    updatedLine
      | snd x <= fst x && fst x <= (snd x) + 2 = line ++ "#"
      | otherwise = line ++ " "
    position = length line
