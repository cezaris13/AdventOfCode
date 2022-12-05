import Data.List.Split
import Data.List
import System.IO
import Control.Monad
import Data.Tuple.Select

type VisitedHouses = [(X,Y,Count)]
type CurrCoordinate = (X,Y)
type X = Int
type Y = Int
type Count = Int

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let oneLine = splitByNewLine contents !! 0
  -- putStrLn $ show $ part1 oneLine [(0,0,1)] (0,0)
  putStrLn $ show $ part2 oneLine [(0,0,1)] (0,0) (0,0) True

part1 :: String -> VisitedHouses -> CurrCoordinate -> Int
part1 [] house _ = length house
part1 (x:xs) house currCoord = part1 xs modifiedHouses modifiedCoord
  where
    modifiedCoord = modifyCoordinates currCoord x
    modifiedHouses = (filter (\(x,y,_) -> not (x == fst modifiedCoord && y == snd modifiedCoord)) house) ++ [updatedHouse]
    coordHouse = filter (\(x,y,_) -> x == fst modifiedCoord && y == snd modifiedCoord ) house
    updatedHouse =
      if length coordHouse == 0
      then
        (fst modifiedCoord, snd modifiedCoord, 1)
      else
        (fst modifiedCoord, snd modifiedCoord, (sel3 $ coordHouse !! 0)+1)

part2 :: String -> VisitedHouses -> CurrCoordinate -> CurrCoordinate -> Bool -> Int
part2 [] house _ _ _ = length house
part2 (x:xs) house currCoordSanta currCoordRobot isSanta = part2 xs modifiedHouses modifiedCoordSanta modifiedCoordRobot (not isSanta)
  where
    modifiedCoordSanta = if isSanta then modifyCoordinates currCoordSanta x else currCoordSanta
    modifiedCoordRobot = if not isSanta then modifyCoordinates currCoordRobot x else currCoordRobot
    modifiedCoord = if isSanta then modifiedCoordSanta else modifiedCoordRobot
    modifiedHouses = (filter (\(x,y,_) -> not (x == fst modifiedCoord && y == snd modifiedCoord)) house) ++ [updatedHouse]
    coordHouse = filter (\(x,y,_) -> x == fst modifiedCoord && y == snd modifiedCoord ) house
    updatedHouse =
      if length coordHouse == 0
      then
        (fst modifiedCoord, snd modifiedCoord, 1)
      else
        (fst modifiedCoord, snd modifiedCoord, (sel3 $ coordHouse !! 0)+1)

modifyCoordinates :: CurrCoordinate -> Char -> CurrCoordinate
modifyCoordinates currCoord x
  | x == '^' = (fst currCoord, snd currCoord + 1)
  | x == 'v' = (fst currCoord, snd currCoord - 1)
  | x == '>' = (fst currCoord + 1, snd currCoord)
  | x == '<' = (fst currCoord - 1, snd currCoord)

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

-- todo part 2
