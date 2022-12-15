import Data.List.Split
import System.IO
import Data.Maybe
import qualified Data.Char as C
import qualified Data.List as L
import Control.Applicative
import Data.Either as E
import GHC.Arr

import Debug.Trace

type Coordinate = (Int,Int)
-- visualization start
toArray :: [[a]] -> Array (Int,Int) a
toArray vss
  = array ((0,0),(h-1,w-1))
    [ ((x,y),v)
    | (x,vs) <- zip [0..] vss
    , (y,v) <- zip [0..] vs
    ]
  where
    w = case vss of
      [] -> 0
      vs:_ -> length vs
    h = length vss

emptyMap :: Int-> Int -> Array (Int,Int) Char
emptyMap x y = toArray [ [ '.' | j <- [1..x] ] | i <- [1..y] ]

mapToStrings :: Array (Int,Int) Char -> [String]
mapToStrings input = L.transpose $ map (\x -> map snd x) grouped
  where
    grouped = L.groupBy (\(x,_) (y,_) -> fst x == fst y) $ assocs input

updateMap :: Array (Int,Int) Char -> [(Coordinate,Char)] -> Array (Int,Int) Char
updateMap input coords = input//coords

-- visualization end

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let commands = filter (\x -> length x > 0 )$ splitByNewLine contents
  let parsedCommands = map splitCommand commands
  let mapp = addCoordsAllCommands parsedCommands
  -- let response =  addWhileAbyss 1 mapp
  let maxY = foldl (\x ((_,y),_) -> max x y) 0 mapp
  let response1 = addWhileAbyss' 1 mapp maxY
  -- putStrLn $ unlines $ getMapInString mapp
  -- putStrLn ""
  -- putStrLn $ unlines $ getMapInString response
  -- print $ show response
  print $ show response1

getMapInString :: [(Coordinate,Char)] -> [String]
getMapInString input = mapToStrings $ updateMap (emptyMap (maxX-minX+1) (maxY+1)) input2
  where
    maxY = foldl (\x ((_,y),_) -> max x y) 0 input
    maxX = foldl (\x ((y,_),_) -> max x y) 0 input
    minX = foldl (\x ((y,_),_) -> min x y) 10000 input
    input2 = map (\((x,y),z)-> ((x-minX,y),z)) input

-- parsing

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

splitCommand :: String -> [Coordinate]
splitCommand input = map (\x -> getCoords $ splitOn "," x) $ splitOn " -> " input

getCoords :: [String] -> Coordinate
getCoords [x,y] = (read x, read y)

addCoordsAllCommands :: [[Coordinate]] -> [(Coordinate,Char)]
addCoordsAllCommands input = addCoordsAllCommands' input []

addCoordsAllCommands' :: [[Coordinate]] -> [(Coordinate,Char)] -> [(Coordinate,Char)]
addCoordsAllCommands' [] response = response
addCoordsAllCommands' (x:xs) coords = addCoordsAllCommands' xs (coords ++ addCoordsOneCommand x)

addCoordsOneCommand :: [Coordinate] -> [(Coordinate,Char)]
addCoordsOneCommand input = L.nub $ addCoordsOneCommand' input []

addCoordsOneCommand' :: [Coordinate] -> [(Coordinate,Char)]-> [(Coordinate,Char)]
addCoordsOneCommand' [x,y] coords
  | fst x == fst y = coords ++ (map (\a -> ((fst y,a),'#')) ([snd x .. snd y] ++ [snd y .. snd x]))
  | otherwise = coords ++ (map (\a -> ((a,snd x),'#')) ([fst x .. fst y] ++ [fst y .. fst x]))
addCoordsOneCommand' (x:xs) coords
  | fst x == fst (xs !! 0) = addCoordsOneCommand' xs (coords ++ (map (\a -> ((fst x,a),'#')) ([snd x .. snd (xs !! 0)] ++ [snd (xs !! 0) .. snd x])))
  | otherwise = addCoordsOneCommand' xs (coords ++ (map (\a -> ((a,snd x),'#')) ([fst x .. fst (xs !! 0) ] ++ [fst (xs !! 0) .. fst x])))

addWhileAbyss :: Int -> [(Coordinate,Char)] -> Int
addWhileAbyss id input
  | isJust oneIteration = trace ("id " ++ show id ++ "\n") addWhileAbyss (id+1) filterr
  | otherwise = id -1
    where
      oneIteration = moveOneSand (500,0) (Just input)
      filterUnusedCoords =  L.nub $ (fromJust oneIteration:input)
      -- filterr = filterUnusedCoords
      filterr = if id `mod` 10 == 0 then L.nub $ filterUnusedCoords' filterUnusedCoords else L.nub filterUnusedCoords

checkIfReachedBottom :: Coordinate -> [(Coordinate,Char)] -> Bool
checkIfReachedBottom input coords = isJust $ L.find (\((x,y),_) -> x == fst input && (snd input)+1 == y) coords

checkIfCanGoLeft :: Coordinate -> [(Coordinate,Char)] -> Bool
checkIfCanGoLeft input coords = not $ isJust $ L.find (\((x,y),_) ->(x == (fst input) - 1 && (snd input)+1 == y)) coords

checkIfCanGoRight :: Coordinate -> [(Coordinate,Char)] -> Bool
checkIfCanGoRight input coords = not $ isJust $ L.find (\((x,y),_) ->( x == (fst input) + 1 && (snd input)+1 == y)) coords

findJustAbove :: Coordinate -> [(Coordinate,Char)] -> Coordinate
findJustAbove (xCoord,yCoord) coords = {--trace (show (xCoord,yCoord) ++ " " ++ show coords ++ "\n" ) --} (xCoord,(foldl (\x y -> if xCoord == fst (fst y) && yCoord < snd (fst y) then min (snd (fst y)) x else x) 10000 coords) -1)

moveOneSand :: Coordinate -> Maybe [(Coordinate,Char)] -> Maybe (Coordinate,Char)
moveOneSand sandGrain Nothing = Nothing
moveOneSand sandGrain (Just coords)
  | not reachedBottom && snd findJustAbove' < 9999 = moveOneSand (findJustAbove (fst sandGrain,snd sandGrain) coords) (Just coords)
  | snd findJustAbove' == 9999 = {-- trace ("final " ++ show sandGrain  ++ " " ++ show coords++ "\n") --} Nothing
  | reachedBottom && canGoLeft = moveOneSand (findJustAbove (fst sandGrain-1,snd sandGrain) coords) (Just coords)
  | reachedBottom && canGoRight = moveOneSand  (findJustAbove (fst sandGrain+1,snd sandGrain) coords) (Just coords)
  | reachedBottom = Just((sandGrain,'O'))
    where
      reachedBottom = checkIfReachedBottom sandGrain coords
      canGoLeft = checkIfCanGoLeft sandGrain coords
      canGoRight = checkIfCanGoRight sandGrain coords
      findJustAbove' = findJustAbove sandGrain coords

filterUnusedCoords' :: [(Coordinate,Char)] -> [(Coordinate,Char)]
filterUnusedCoords' input = L.reverse $ filterUnusedCoords'' a []
  where a = L.reverse $ L.sortBy (\((_,x),_) ((_,y),_)-> compare x y) input

filterUnusedCoords'' :: [(Coordinate,Char)] -> [(Coordinate,Char)] -> [(Coordinate,Char)]
filterUnusedCoords'' [] acc = acc
filterUnusedCoords'' (xs:input) acc = filterUnusedCoords'' input (if not filtered then xs:acc else acc)
  where
    filtered = elem up filteredMap && elem leftUp filteredMap && elem rightUp filteredMap
    filteredMap = map (\(x,y) -> x) (input)
    up = ((fst $ fst xs),(snd $ fst xs)-1)
    leftUp = ((fst $ fst xs)-1,(snd $ fst xs)-1)
    rightUp = ((fst $ fst xs)+1,(snd $ fst xs)-1)

-- port 2

moveOneSand' :: Coordinate -> Maybe [(Coordinate,Char)] -> Int -> Maybe (Coordinate,Char)
moveOneSand' sandGrain Nothing _ = Nothing
moveOneSand' sandGrain (Just coords) maxY
  | not reachedBottom && snd findJustAbove' < 9999 = moveOneSand' (findJustAbove (fst sandGrain,snd sandGrain) coords) (Just coords) maxY
  | snd findJustAbove' == 9999 = {-- trace ("final " ++ show sandGrain  ++ " " ++ show coords++ "\n") --} Just ((fst sandGrain,maxY-1),'O')
  | reachedBottom && canGoLeft = moveOneSand' (findJustAbove (fst sandGrain-1,snd sandGrain) coords) (Just coords) maxY
  | reachedBottom && canGoRight = moveOneSand'  (findJustAbove (fst sandGrain+1,snd sandGrain) coords) (Just coords) maxY
  | reachedBottom = Just((sandGrain,'O'))
    where
      reachedBottom = checkIfReachedBottom sandGrain coords
      canGoLeft = checkIfCanGoLeft sandGrain coords
      canGoRight = checkIfCanGoRight sandGrain coords
      findJustAbove' = findJustAbove sandGrain coords

addWhileAbyss' :: Int -> [(Coordinate,Char)] -> Int -> Int
addWhileAbyss' id input maxY
  | isJust oneIteration && (fst $ fromJust oneIteration) /= (500,0)  = trace ("id " ++ show id ++ "\n") addWhileAbyss' (id+1) filterr maxY
  | otherwise = id
    where
      oneIteration = moveOneSand' (500,0) (Just input) (maxY + 2)
      filterUnusedCoords =  L.nub $ (fromJust oneIteration):input
      -- filterr = filterUnusedCoords
      filterr = if id `mod` 10 == 0 then L.nub $ filterUnusedCoords' filterUnusedCoords else L.nub filterUnusedCoords
