import Data.List.Split
import Data.List
import System.IO
import Control.Monad
import Data.Maybe
import Data.Tuple.Select
import Data.Tuple.Update

type Command = (Int,Int,Int)
type Blocks = [Block]
type Block = (Int, [Char])

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = filter (\x -> length x > 2) $ splitByNewLine contents
  print $ part1 content
  print $ part2 content

part1 :: [String] -> String
part1 input = map (\(x,y) -> y !! 0) finalBlocks
  where
    dividedData = splitData input
    blocks = parseToBlocks $ fst dividedData
    commands = map stringToCommand $ snd dividedData
    finalBlocks = sortBy (\(a,_) (b,_) -> compare a b) $ executeAllCommands commands blocks

part2 :: [String] -> String
part2 input = map (\(x,y) -> y !! 0) finalBlocks
  where
    dividedData = splitData input
    blocks = parseToBlocks $ fst dividedData
    commands = map stringToCommand $ snd dividedData
    finalBlocks = sortBy (\(a,_) (b,_) -> compare a b) $ executeAllCommandsPT2 commands blocks

-- Command parsing
splitData :: [String] -> ([String],[String])
splitData input = (takeWhile splitDataFunction input, dropWhile splitDataFunction input)
  where
    splitDataFunction = not . isInfixOf "move"

stringToCommand :: String -> Command
stringToCommand input = listToTuple $ map convertStringToInt $ takeEvery 2 $ splitOn " " input

takeEvery :: Int -> [String] -> [String]
takeEvery n = map snd . filter ((==n) . fst) . zip (cycle [1..n])

convertStringToInt :: String -> Int
convertStringToInt = read

listToTuple :: [Int] -> Command
listToTuple [a,b,c] = (a,b,c)

-- Block parsing

parseToBlocks :: [String] -> Blocks
parseToBlocks input = parseToBlocks' (reverse input) []

parseToBlocks' :: [String] -> Blocks -> Blocks -- let take from other side
parseToBlocks' [] blocks = blocks
parseToBlocks' (x:xs) blocks
  | not $ elem '[' x = parseToBlocks' xs (addInitialValues listOfInts)
  | otherwise = parseToBlocks' xs (addValuesToBlocks blocks x)
  where
    listOfInts = map convertStringToInt $ filter (\a -> length a > 0) $ splitOn " " x

addInitialValues :: [Int] -> Blocks
addInitialValues l = map (\x -> (x,[])) l

addValuesToBlocks :: Blocks -> String -> Blocks
addValuesToBlocks blocks input = convertToBlocks' (convertStringToBlocks input) 1 blocks

convertStringToBlocks :: String -> [String]
convertStringToBlocks input =concat $map (splitOn "]") $ splitOn "[" input

convertToBlocks' :: [String] -> Int -> Blocks -> Blocks
convertToBlocks' [] _ blocks = blocks
convertToBlocks' (x:xs) id blocks
  | elem ' ' x = convertToBlocks' xs (id+((length x) `div` 4)) blocks
  | length x > 0 = convertToBlocks' xs (succ id) filteredBlocks
  | otherwise = convertToBlocks' xs id blocks
  where
    filteredBlocks = (filter (\x -> sel1 x /= id) blocks) ++ appendedBlock
    blockById = (filter (\x -> sel1 x == id) blocks) !! 0
    appendedBlock = [((sel1 blockById), x ++ (sel2 blockById))]
-- Parsing end

-- Command execution
executeOneCommand :: Blocks -> Command -> Blocks
executeOneCommand blocks command = filteredByElements ++ [((sel2 command),fromBlocksRemove)] ++ [((sel3 command),toBlocksAndUpdate)]
  where
    fromBlocks = fromJust $ lookup (sel2 command) blocks
    toBlocksAndUpdate = reverse (take (sel1 command) fromBlocks) ++ (fromJust $ lookup (sel3 command) blocks)
    fromBlocksRemove = drop (sel1 command) fromBlocks
    filteredByElements = filter (\x->(sel1 x) /= (sel2 command) && (sel1 x) /= (sel3 command)) blocks

executeAllCommands :: [Command] -> Blocks -> Blocks
executeAllCommands [] blocks = blocks
executeAllCommands (x:xs) blocks = executeAllCommands xs $ executeOneCommand blocks x

executeAllCommandsPT2 :: [Command] -> Blocks -> Blocks
executeAllCommandsPT2 [] blocks = blocks
executeAllCommandsPT2 (x:xs) blocks = executeAllCommandsPT2 xs $ executeOneCommandPT2 blocks x

executeOneCommandPT2 :: Blocks -> Command -> Blocks
executeOneCommandPT2 blocks command = filteredByElements ++ [((sel2 command),fromBlocksRemove)] ++ [((sel3 command),toBlocksAndUpdate)]
  where
    fromBlocks = fromJust $ lookup (sel2 command) blocks
    toBlocksAndUpdate = take (sel1 command) fromBlocks ++ (fromJust $ lookup (sel3 command) blocks)
    fromBlocksRemove = drop (sel1 command) fromBlocks
    filteredByElements = filter (\x->(sel1 x) /= (sel2 command) && (sel1 x) /= (sel3 command)) blocks
-- Execution end

-- Misc functions
splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"
-- Misc end
