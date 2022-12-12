import Data.List.Split
import System.IO
import Data.Maybe
import Data.Char
import GHC.Arr
import qualified Data.List as L
import qualified Data.Set as S

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

type Coords = (Int,Int)
type Visited = (Int,Int)
type Map = Array Coords Char

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = L.filter (\x -> length x > 0) $ splitByNewLine contents
  print content
  let arrayOfMountains = toArray content
  print $ part1 arrayOfMountains
  print $ part2 arrayOfMountains

part1 :: Map -> Int
part1 mapp = snd $ filter (\x -> fst x == endPos) (part1bfs mapp startPos) !! 0
  where
     startPos = fst $ (L.filter (\x -> snd x == 'S') $ assocs mapp) !! 0
     endPos = fst $ (L.filter (\x -> snd x == 'E') $ assocs mapp) !! 0

part2 :: Map -> Int
part2 mapp = (L.sort $ map snd $ filter (\x -> elem (fst x) aS) $ part2bfs mapp endPos) !! 0
  where
     startPos = fst $ (L.filter (\x -> snd x == 'S') $ assocs mapp) !! 0
     endPos = fst $ (L.filter (\x -> snd x == 'E') $ assocs mapp) !! 0
     aS = map fst $ filter (\x -> snd x == 'a' || snd x == 'S') $ assocs mapp

part1bfs :: Map -> Coords -> [(Coords,Int)] -- if you want to you can print what partNbfs returns
part1bfs mapp startPos = bfs (S.singleton (0,startPos)) [startPos] [(startPos,0)] mapp allowedNeighbours

part2bfs :: Map -> Coords -> [(Coords,Int)]
part2bfs mapp endPos = bfs (S.singleton (0,endPos)) [endPos] [(endPos,0)] mapp allowedNeighbours'

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

bfs :: S.Set (Int,Coords) -> [Visited] -> [(Coords,Int)] -> Map -> (Char -> [Char]) -> [(Coords,Int)]
bfs positionsToVisit _  coords _ _
  | S.null positionsToVisit = coords
bfs positionsToVisit visited accCoords mapp func = bfs updatedPosToVisit updatedVisited updatedCoords mapp func
  where
    updatedPosToVisit = addValuesToSet (deleteFirstSetElement positionsToVisit) (map (\x ->(distance+1,x)) validNeighbours)
    validNeighbours = getNotVisitedNeighbours visited $ filterValidNeighbours currCoord mapp func
    updatedVisited =
      if notElem currCoord visited
      then
        visited ++ [currCoord]
      else
        visited
    updatedCoords = accCoords ++ (filter (\x -> notElem x accCoords)  (map (\x -> (x,distance+1)) validNeighbours))
    (distance, currCoord) =  S.elemAt 0 positionsToVisit

filterValidNeighbours :: Coords -> Map -> (Char -> [Char]) -> [Coords]
filterValidNeighbours coord mapp filterFunc = map fst filteredNeighbours
  where
    validNeighbours = filterFunc $ snd $ getPositionValue coord mapp
    neighbours = map (\x -> getPositionValue x mapp) $ filterOnGrid (getNeighbours coord) mapp
    filteredNeighbours = filter (\x -> elem (snd x) validNeighbours) neighbours

allowedNeighbours :: Char -> [Char]
allowedNeighbours 'S' = ['a', 'b', 'E', 'S']
allowedNeighbours 'y' = map chr [(ord 'a') .. (ord 'z')] ++ ['E', 'S']
allowedNeighbours 'z' = map chr [(ord 'a') .. (ord 'z')] ++ ['E', 'S']
allowedNeighbours c = map chr [(ord 'a') .. (ord c)] ++ [chr $ ord c + 1, 'S']

allowedNeighbours' :: Char -> [Char]
allowedNeighbours' 'E' = ['y','z','E', 'S']
allowedNeighbours' 'y' = map chr [(ord 'x') .. (ord 'z')] ++ ['E','S']
allowedNeighbours' 'z' = map chr [(ord 'y') .. (ord 'z')] ++ ['E','S']
allowedNeighbours' c = map chr [(ord c) .. (ord 'z')] ++ [chr $ ord c-1,'S']

-- general functions

filterOnGrid :: [Coords] -> Map -> [Coords]
filterOnGrid coords mapp = filter (\x -> elem x (map fst $ assocs mapp)) coords

getNeighbours :: Coords -> [Coords]
getNeighbours (x,y) = [(x+1, y), (x-1,y), (x, y+1), (x, y-1)]

getPositionValue :: Coords -> Map -> (Coords,Char)
getPositionValue coord map = fromJust $ L.find (\x -> fst x == coord) $ assocs map

getNotVisitedNeighbours :: [Visited] -> [Coords] -> [Coords]
getNotVisitedNeighbours visited coords = L.filter (\x -> notElem x visited) coords

addValuesToSet :: S.Set (Int,Coords) -> [(Int,Coords)] -> S.Set (Int,Coords)
addValuesToSet set coords = foldl (\x y -> S.insert y x) set coords

deleteFirstSetElement :: S.Set (Int,Coords) -> S.Set (Int,Coords)
deleteFirstSetElement input = S.delete (S.elemAt 0 input) input
