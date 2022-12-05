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
  putStrLn $ show $ part1 oneLine [(0,0,1)] (0,0)

part1 :: String -> VisitedHouses -> CurrCoordinate -> Int
part1 [] house _ = length house
part1 (x:xs) house currCoord = part1 xs modifiedHouses modifiedCoord
  where
    modifiedCoord
      | x == '^' = (fst currCoord, snd currCoord + 1)
      | x == 'v' = (fst currCoord, snd currCoord - 1)
      | x == '>' = (fst currCoord + 1, snd currCoord)
      | x == '<' = (fst currCoord - 1, snd currCoord)
    modifiedHouses = (filter (\(x,y,_) -> not (x == fst modifiedCoord && y == snd modifiedCoord)) house) ++ [updatedHouse]
    coordHouse = filter (\(x,y,_) -> x == fst modifiedCoord && y == snd modifiedCoord ) house
    updatedHouse =
      if length coordHouse == 0
      then
        (fst modifiedCoord, snd modifiedCoord, 1)
      else
        (fst modifiedCoord, snd modifiedCoord, (sel3 $ coordHouse !! 0)+1)

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

-- todo part 2
