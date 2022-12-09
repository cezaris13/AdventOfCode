import Data.List.Split
import System.IO

type Command = (String,Int)
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
    c = take 1 input
    parsedInt = parseStringToInt $ drop 2 input

parseStringToInt :: String -> Int
parseStringToInt = read

moveRope :: CoordsVisited -> Command -> CoordsVisited
moveRope coords (_,0) = coords
moveRope coords (command,n) =moveRope updatedCoords (command,n-1)
  where
    updatedCoords = updateCoordinatesOneCommand coords command

updateCoordinatesOneCommand :: CoordsVisited -> String -> CoordsVisited
updateCoordinatesOneCommand coords comm = func' coords comm ([],[])

func' :: CoordsVisited -> String -> CoordsVisited -> CoordsVisited
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
func' (x:xs,vis) command ([],[]) =func' (xs,vis) command ([moveElement x (command,1)],vis)
func' (x:xs,visited) command newCoords = func' (xs,visited) (fst response) (fst newCoords ++ [updatedX],snd newCoords)
  where
    updatedX
      | chebyshevDistance x (last $ fst newCoords) > 1 = snd response
      | otherwise = x
    response =  updateX x (last $ fst newCoords) command

updateX :: Coordinate -> Coordinate -> String -> (String,Coordinate)
updateX (x1,y1) (px2,py2) command
  | length command == 1 && (x1-px2 == 0 || y1 - py2 == 0) = (command, moveElement (x1,y1) (command,1))
  | x1-px2 == 0 && length command == 2 = (drop 1 command, moveElement (x1,y1) (drop 1 command,1))
  | y1-py2 == 0 && length command == 2 = (take 1 command, moveElement (x1,y1) (take 1 command,1))
  | x1-px2 == 0 && y1-py2 == 0 = (command, (x1,y1) )
  | otherwise = updatedCommand
  where
    updatedCommand
      | x1 - px2 > 0 && y1 - py2 > 0 = ("LD",(x1-1,y1-1))
      | x1 - px2 < 0 && y1 - py2 > 0 = ("RD",(x1+1,y1-1))
      | x1 - px2 > 0 && y1 - py2 < 0 = ("LU",(x1-1,y1+1))
      | x1 - px2 < 0 && y1 - py2 < 0 = ("RU",(x1+1,y1+1))

moveElement :: Coordinate -> Command -> Coordinate
moveElement (x,y) (c,val)
  | c == "R" = (x+val,y)
  | c == "L" = (x-val,y)
  | c == "U" = (x,y+val)
  | c == "D" = (x,y-val)

chebyshevDistance :: Coordinate -> Coordinate -> Int
chebyshevDistance (x1,y1) (x2,y2) = max (abs (x1-x2)) (abs (y1-y2))
