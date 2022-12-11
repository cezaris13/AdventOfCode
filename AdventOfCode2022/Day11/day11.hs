import Data.List.Split
import Data.List
import System.IO
import Data.Char
import Data.Tuple.Select
import Data.Tuple.Update
import Data.Maybe

type MonkeyData = (MonkeyId,Items,InspectedCount,Operation,Test)
type Test = (DivisibleBy, IfTrue,IfFalse)
type DivisibleBy = Int
type IfTrue = MonkeyId
type IfFalse = MonkeyId
type MonkeyId = Int
type Items = [Int]
type InspectedCount = Int
type Operation = (Char, Element)
type Element = Maybe Int -- if Nothing - old self

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = filter (\x -> length x > 0) $ splitOn [""] $ splitByNewLine contents
  let parsedMonkeys = map (\x -> parseMonkey x (0,[],0,('$',Nothing),(0,0,0))) content
  print $ part1 parsedMonkeys
  print $ part2 parsedMonkeys
-- parsing

part1 :: [MonkeyData] -> Int
part1 monkeys = foldl(\x y -> x*y) 1 $ take 2 $ reverse $ sort $ map sel3 monkeys20
  where
    monkeys20 = allMonkeyTurnsNTimes 20 monkeys True lcmMonkeys
    lcmMonkeys = lcmN $ map (\x -> sel1 $ sel5 x) monkeys

part2 :: [MonkeyData] -> Int
part2 monkeys = foldl(\x y -> x*y) 1 $ take 2 $ reverse $ sort $ map sel3 monkeys10000
  where
    monkeys10000 = allMonkeyTurnsNTimes 10000 monkeys False lcmMonkeys
    lcmMonkeys = lcmN $ map (\x -> sel1 $ sel5 x) monkeys

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

parseMonkey :: [String] -> MonkeyData -> MonkeyData
parseMonkey [] monkey = monkey
parseMonkey (x:xs) monkey
  | isInfixOf "Monkey" x = parseMonkey xs (upd1 (getNumber x) monkey)
  | isInfixOf "Starting" x = parseMonkey xs (upd2 (getListOfInts x) monkey)
  | isInfixOf "Operation" x = parseMonkey xs (upd4 (parseOperation x) monkey)
  | isInfixOf "Test" x = parseMonkey xs (upd5 (getNumber x,0,0) monkey)
  | isInfixOf "If true" x = parseMonkey xs (upd5 (upd2 (getNumber x) (sel5 monkey)) monkey)
  | isInfixOf "If false" x =  parseMonkey xs (upd5 (upd3 (getNumber x) (sel5 monkey)) monkey)

getNumber :: String -> Int
getNumber input = convertStringToInt $ takeWhile isDigit $ dropWhile (\x -> not $ isDigit x) input

convertStringToInt :: String -> Int
convertStringToInt = read

getListOfInts :: String -> [Int]
getListOfInts input = map convertStringToInt $ splitOn "," $ dropWhile (\x -> not $ isDigit x) input

parseOperation :: String -> Operation
parseOperation input = (sign, value)
  where
    sign = operation !! 0
    value
      | isInfixOf "old" operation = Nothing
      | otherwise = Just (getNumber operation)
    operation = drop (length "= old ") $ dropWhile (\x -> x /= '=') input

-- parsing end

newValue :: Int -> Operation -> Int
newValue oldVal (sign,val)
  | isJust val && sign == '*' = oldVal * (fromJust val)
  | isJust val && sign == '+' = oldVal + (fromJust val)
  | sign == '*' = oldVal * oldVal
  | sign == '+' = 2*oldVal

passValueToMonkey :: Int -> Test -> MonkeyId
passValueToMonkey value (div,true,false)
  | value `mod` div == 0 = true
  | otherwise = false

allMonkeyTurnsNTimes :: Int -> [MonkeyData] -> Bool -> Int -> [MonkeyData]
allMonkeyTurnsNTimes id monkeys shouldReduce lcmV
  | id > 0 = allMonkeyTurnsNTimes (id-1) (allMonkeyTurns monkeys shouldReduce lcmV) shouldReduce lcmV
  | otherwise = monkeys

allMonkeyTurns :: [MonkeyData] -> Bool -> Int -> [MonkeyData]
allMonkeyTurns = allMonkeyTurns' 0

allMonkeyTurns' :: MonkeyId -> [MonkeyData] -> Bool -> Int -> [MonkeyData]
allMonkeyTurns' id monkeys shouldReduce lcmV
  | length monkeys <= id = monkeys
  | otherwise = allMonkeyTurns' (id+1) (oneMonkeyTurn (monkeys !! id) monkeys shouldReduce lcmV) shouldReduce lcmV

oneMonkeyTurn :: MonkeyData -> [MonkeyData] -> Bool -> Int -> [MonkeyData]
oneMonkeyTurn (_,[],_,_,_) monkeys _ _ = monkeys
oneMonkeyTurn (id,(x:xs),inspectedCount,op,test) monkeys shouldReduce lcmV = oneMonkeyTurn updatedCurrMonkey updatedMonkeys shouldReduce lcmV
  where
    newVal = newValue x op
    reducedVal
      | shouldReduce = newVal `div` 3
      | otherwise = newVal `mod` lcmV
    monkeyWhoGets = passValueToMonkey reducedVal test
    updatedMonkeys = foldl (\x y ->
                              if (sel1 y) == id
                              then
                                x++[updatedCurrMonkey]
                              else (
                                if (sel1 y) == monkeyWhoGets
                                then
                                  x++[upd2 ((sel2 y)++[reducedVal]) y]
                                else
                                  x++[y])) [] monkeys
    updatedCurrMonkey = (id,xs,inspectedCount+1,op,test)


lcmN :: [Int] -> Int
lcmN input = lcmN' input 1

lcmN' :: [Int] -> Int -> Int
lcmN' [] ans = ans
lcmN' (x:xs) val = lcmN' xs (lcm val x)
