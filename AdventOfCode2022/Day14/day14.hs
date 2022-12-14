import Data.List.Split
import System.IO
import Data.Maybe
import qualified Data.Char as C
import qualified Data.List as L
import Control.Applicative
import Data.Either as E

import Debug.Trace

type Coordinate = (Int,Int)

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let commands = filter (\x -> length x > 0 )$ splitByNewLine contents
  let parsedCommands = map splitCommand commands
  let mapp = addCoordsAllCommands parsedCommands
  let lowestPoint = (reverse $ L.sort $ map (\(x,y) -> snd x) mapp) !! 0
  print $ addWhileAbyss 1 lowestPoint mapp

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

addWhileAbyss :: Int -> Int -> [(Coordinate,Char)] -> Int
addWhileAbyss id lowestPoint input
  | isJust oneIteration = trace ("id " ++ show id ++ "\n") addWhileAbyss (id+1) lowestPoint ((fromJust oneIteration):input)
  | otherwise = id + 1
    where
      oneIteration = moveOneSand (500,0) lowestPoint (Just input)

checkIfReachedBottom :: Coordinate -> [(Coordinate,Char)] -> Bool
checkIfReachedBottom input coords = isJust $ L.find (\((x,y),_) -> x == fst input && (snd input)+1 == y) coords

checkIfCanGoLeft :: Coordinate -> [(Coordinate,Char)] -> Bool
checkIfCanGoLeft input coords = not $ isJust $ L.find (\((x,y),_) -> (x == (fst input) - 1 && (snd input) == y) || (x == (fst input) - 1 && (snd input)+1 == y)) coords

checkIfCanGoRight :: Coordinate -> [(Coordinate,Char)] -> Bool
checkIfCanGoRight input coords = not $ isJust $ L.find (\((x,y),_) -> (x == (fst input) + 1 && (snd input) == y) || ( x == (fst input) + 1 && (snd input)+1 == y)) coords

findJustAbove :: Int -> [(Coordinate,Char)] -> Int -- x -> y
findJustAbove xCoord coords = foldl (\x y -> if xCoord == fst (fst y) then min (snd (fst y)) x else x) 10000 coords

moveOneSand :: Coordinate -> Int -> Maybe [(Coordinate,Char)] -> Maybe (Coordinate,Char)
moveOneSand sandGrain lowestPoint Nothing = Nothing
moveOneSand sandGrain lowestPoint (Just coords)
  | not reachedBottom && snd sandGrain <= lowestPoint = moveOneSand (fst sandGrain,snd sandGrain+1) lowestPoint (Just coords)
  | not reachedBottom && lowestPoint < (snd sandGrain) = trace ("final " ++ show coords ++ "\n") Nothing
  | reachedBottom && canGoLeft = moveOneSand (fst sandGrain-1,snd sandGrain+1) lowestPoint (Just coords)
  | reachedBottom && canGoRight = moveOneSand  (fst sandGrain+1,snd sandGrain+1) lowestPoint (Just coords)
  | reachedBottom = Just((sandGrain,'O'))
    where
      reachedBottom = checkIfReachedBottom sandGrain coords
      canGoLeft = checkIfCanGoLeft sandGrain coords
      canGoRight = checkIfCanGoRight sandGrain coords
