import Data.List.Split
import Data.List
import System.IO
import Control.Monad
import Data.Maybe
import Data.Tuple.Select
import Data.Char
import GHC.Integer
import GHC.Arr (Array, array, bounds, assocs)
import Debug.Trace

type Trees = Array (Int,Int) Int

type Response = ((Int,Int),Int)

toArray :: [[a]] -> Array (Int,Int) a
toArray vss
  = array ((0,0),(w-1,h-1))
    [ ((x,y),v)
    | (x,vs) <- zip [0..] vss
    , (y,v) <- zip [0..] vs
    ]
  where
    w = case vss of
      [] -> 0
      vs:_ -> length vs
    h = length vss

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = map (\x -> map digitToInt x) $ filter (\x -> length x > 2) $ splitByNewLine contents
  let arrayOfTrees = toArray content
  print arrayOfTrees
  -- print $ length $ solution arrayOfTrees
  print $ maximum $ map (\(_,y) -> y) $ solution1 arrayOfTrees

solution :: Trees -> [(Int,Int)]
solution trees = solution' 0 0 trees []

solution' :: Int -> Int -> Trees -> [(Int,Int)] -> [(Int,Int)]
solution' x y trees accumulatedCoords
  | y == maxYSize && x == maxXSize = updatedCoords
  | y == maxYSize && x /= maxXSize = solution' (x+1) 0 trees updatedCoords
  | otherwise = solution' x (y+1) trees updatedCoords
  where
    maxXSize = fst $ snd $ bounds trees
    maxYSize = snd $ snd $ bounds trees
    updatedCoords = if checkIfTreeIsVisible x y trees then accumulatedCoords ++ [(x,y)] else accumulatedCoords

checkIfTreeIsVisible :: Int -> Int -> Trees -> Bool
checkIfTreeIsVisible x y trees
  | x == 0 = True
  | y == 0 = True
  | x == maxXSize = True
  | y == maxYSize = True
  | otherwise = checkIfHigher valueAtCoord valuesU || checkIfHigher valueAtCoord valuesR || checkIfHigher valueAtCoord valuesL  || checkIfHigher valueAtCoord valuesD
  where
    maxXSize = fst $ snd $ bounds trees
    maxYSize = snd $ snd $ bounds trees
    valuesU = collectAllValuesU trees x y
    valuesD = collectAllValuesD trees x y
    valuesR = collectAllValuesR trees x y
    valuesL = collectAllValuesL trees x y
    valueAtCoord = getHeightAtCoord trees x y

collectAllValuesL :: Trees -> Int -> Int -> [Int]
collectAllValuesL trees x y = map snd $ filter (\(z,_) -> fst z < x && snd z == y) $ assocs trees

collectAllValuesR :: Trees -> Int -> Int -> [Int]
collectAllValuesR trees x y = map snd $ filter (\(z,_) -> fst z > x && snd z == y) $ assocs trees

collectAllValuesU :: Trees -> Int -> Int -> [Int]
collectAllValuesU trees x y = map snd $ filter (\(z,_) -> fst z == x && snd z < y) $ assocs trees

collectAllValuesD :: Trees -> Int -> Int -> [Int]
collectAllValuesD trees x y = map snd $ filter (\(z,_) -> fst z == x && snd z > y) $ assocs trees

getHeightAtCoord :: Trees -> Int -> Int -> Int
getHeightAtCoord trees x y = snd $ fromJust $  find (\(z,_) -> fst z == x && snd z == y) $ assocs trees

checkIfHigher :: Int -> [Int] -> Bool
checkIfHigher height heights
  | isJust $ find (\x -> height <= x) heights = False
  | otherwise = True

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

-- part2

collectAllTreesL :: Trees -> Int -> Int -> [((Int,Int),Int)]
collectAllTreesL trees x y = reverse $ filter (\(z,_) -> fst z < x && snd z == y) $ assocs trees

collectAllTreesR :: Trees -> Int -> Int -> [((Int,Int),Int)]
collectAllTreesR trees x y = filter (\(z,_) -> fst z > x && snd z == y) $ assocs trees

collectAllTreesU :: Trees -> Int -> Int -> [((Int,Int),Int)]
collectAllTreesU trees x y = reverse $ filter (\(z,_) -> fst z == x && snd z < y) $ assocs trees

collectAllTreesD :: Trees -> Int -> Int -> [((Int,Int),Int)]
collectAllTreesD trees x y = filter (\(z,_) -> fst z == x && snd z > y) $ assocs trees

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

countVisibleTrees :: Trees -> Int -> Int -> Response
countVisibleTrees trees x y =((x,y), (left*right*up* down))
  where
    left = length $ takeWhileOneMore (\(_,y) -> y < val) $ collectAllTreesL trees x y
    right = length $ takeWhileOneMore (\(_,y) -> y < val) $ collectAllTreesR trees x y
    up = length $ takeWhileOneMore (\(_,y) -> y < val) $ collectAllTreesU trees x y
    down = length $ takeWhileOneMore (\(_,y) -> y < val) $ collectAllTreesD trees x y
    val = getHeightAtCoord trees x y

solution1 :: Trees -> [Response]
solution1 trees = solution1' 0 0 trees []

solution1' :: Int -> Int -> Trees -> [Response] -> [Response]
solution1' x y trees values
  | y == maxYSize && x == maxXSize = values
  | y == maxYSize && x /= maxXSize = solution1' (x+1) 0 trees values
  | x == maxXSize && maxYSize /= y = solution1' x (y+1) trees values
  | y == 0 = solution1' x 1 trees values
  | x == 0 = solution1' 0 (y+1) trees values
  | otherwise = solution1' x (y+1) trees updatedVals
  where
    maxXSize = fst $ snd $ bounds trees
    maxYSize = snd $ snd $ bounds trees
    updatedVals = values ++ [countVisibleTrees trees x y]
