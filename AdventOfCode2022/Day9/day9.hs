import Data.List.Split
import Data.List
import System.IO
import Control.Monad
import Data.Maybe
import Data.Tuple.Select
import Data.Char
import GHC.Integer
import Debug.Trace

type Command = (Char,Int)
type Coordinate = (Int,Int)

type CoordsVisited = ([Coordinate],[Coordinate])

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let content = map parseData $ filter (\x -> length x > 0) $ splitByNewLine contents
  let response = foldl (\x y -> moveRope x y) (map (\_ -> (0,0)) [1..2],[(0,0)]) content -- part 1
  print $ length $ snd response
  let response = foldl (\x y -> moveRope x y) (map (\_ -> (0,0)) [1..10],[(0,0)]) content -- part 2
  print $ length $ snd response

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

parseData :: String -> Command
parseData input = (c,parsedInt)
  where
    c = input !! 0
    parsedInt = parseStringToInt $ drop 2 input

parseStringToInt :: String -> Int
parseStringToInt = read

moveRope :: CoordsVisited -> Command -> CoordsVisited
moveRope coords (_,0) = coords
moveRope coords (command,n) = moveRope updatedCoords (command,n-1)
  where
    updatedCoords = updateCoordinatesOneCommand coords command

updateCoordinatesOneCommand :: CoordsVisited -> Char -> CoordsVisited
updateCoordinatesOneCommand coords comm = func' coords comm ([],[])

func' :: CoordsVisited -> Char -> CoordsVisited -> CoordsVisited -- save only last list element positions
func' ([],_) _ coords = coords
func' ([x],visited) command newCoords = (fst newCoords ++ [updatedX], updateVisitedCoords)
  where
    updatedX
      | chebyshevDistance x (last $ fst newCoords) > 1 = snd response
      | otherwise = x
    response =  updateX x (last $ fst newCoords) command
    updateVisitedCoords
      | elem updatedX (snd newCoords) = snd newCoords
      | otherwise = snd newCoords ++ [updatedX]
func' (x:xs,vis) command ([],[]) = func' (xs,vis) command ([moveElement x (command,1)],vis)
func' (x:xs,visited) command newCoords = func' (xs,visited) (fst response) (fst newCoords ++ [updatedX],snd newCoords)
  where
    updatedX
      | chebyshevDistance x (last $ fst newCoords) > 1 = snd response
      | otherwise = x
    response =  updateX x (last $ fst newCoords) command

updateX :: Coordinate -> Coordinate -> Char -> (Char,Coordinate)
updateX (x1,y1) (px2,py2) command
  | x1-px2 == 0 || y1-py2 == 0 = (command, moveElement (x1,y1) (command,1))
  | otherwise = updatedCommand
  where
    updatedCommand
      | x1 - px2 > 0 && y1 - py2 > 0 = (command,(x1-1,y1-1)) -- these seem to have an effect
      | x1 - px2 < 0 && y1 - py2 > 0 = (command,(x1+1,y1-1)) -- think of edge cases
      | x1 - px2 > 0 && y1 - py2 < 0 = (command,(x1-1,y1+1))
      | x1 - px2 < 0 && y1 - py2 < 0 = (command,(x1+1,y1+1))

moveElement :: Coordinate -> Command -> Coordinate
moveElement (x,y) (c,val)
  | c == 'R' = (x+val,y)
  | c == 'L' = (x-val,y)
  | c == 'U' = (x,y+val)
  | c == 'D' = (x,y-val)

chebyshevDistance :: Coordinate -> Coordinate -> Int
chebyshevDistance (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))

-- 1536 < 2304 < z < 3072
--2592?

-- 2688
