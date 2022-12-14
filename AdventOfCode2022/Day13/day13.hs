{-# LANGUAGE FlexibleInstances #-}
import Data.List.Split
import System.IO
import Data.Maybe
import qualified Data.Char as C
import qualified Data.List as L
import Control.Applicative
import Data.Either as E

import Debug.Trace

data BoolPlus
  = Truey
  | Falsey
  | Equal
  deriving(Show, Eq)

data Json
  = JsonInteger Int
  | JsonList [Json]
  | JsonNull
  deriving (Show)

main :: IO()
main = do
  file <- openFile "input" ReadMode
  contents <- hGetContents file
  let parseStrings = parseString' contents
  let ans = map compare' parseStrings
  putStrLn $ unlines $ map show ans
  print $ foldl (\x y -> x+fst y) 0 $ filter (\(_,x) -> x == Truey || x == Equal) $ zip [1..] ans
  -- print $zip parseStrings ans

splitByNewLine :: String -> [String]
splitByNewLine = splitOn "\n"

parseString' :: String -> [(Json,Json)]
parseString' input = parsedJson
  where
    splitByNewline' = splitByNewLine input
    splitByEmptyLines = filter (\x -> length x > 0) $ splitOn [""] splitByNewline'
    parsedJson = map (\x -> (parsed x!!0,parsed x !! 1)) splitByEmptyLines
                        where
                         parsed = map (\y -> E.fromRight JsonNull $ parseJsonMessage y)

compare' :: (Json, Json) -> BoolPlus
compare' (JsonInteger a, JsonInteger b) = trace ("comparing ints: " ++ show a ++ " "++ show b ++"\n")compareInts (JsonInteger a,JsonInteger b)
compare' (JsonList a, JsonInteger b) = trace ("comparing list int: " ++ show a ++ " " ++ show b ++ "\n") compareListInt (JsonList a,JsonInteger b)
compare' (JsonInteger a, JsonList b) = trace ("comparing int list: " ++ show a ++ " " ++ show b ++ "\n") compareIntList (JsonInteger a, JsonList b)
compare' (JsonList (x:xs), JsonList (y:ys)) =
  if
    response == Equal
  then
     (trace ("equal: "++ show x ++ " " ++ show y ++ "\n") compare' (JsonList xs, JsonList ys))
  else
     (trace ("notEqual " ++ show x ++ " " ++ show y ++ "\n" ) response)
    where
      response = compare' (x,y)
compare' (JsonList [], JsonList []) = Equal
compare' (JsonList a, JsonList []) = Falsey
compare' (JsonList [], JsonList a) = Truey

compareInts :: (Json,Json) -> BoolPlus
compareInts (JsonInteger a, JsonInteger b )
  | a<b = Truey
  | a==b = trace ("helo") Equal
  | otherwise = Falsey

compareListInt :: (Json,Json) -> BoolPlus
compareListInt (JsonList [], JsonInteger _) = Truey
compareListInt (JsonList [a], JsonInteger b) =
  case a of
    JsonList c -> if length c == 0 then Truey else compareListInt (JsonList [c !! 0], JsonInteger b)
    JsonInteger _ -> compareInts (a,JsonInteger b)
compareListInt (JsonList a, JsonInteger b)
  | length a == 0 = Truey
  | otherwise = ans
  where
    firstElem = a !! 0
    ans =
      case firstElem of
        JsonInteger c -> if (result == Equal && length a > 1) then Falsey else result
          where
            result = compareInts (firstElem, JsonInteger b)
        JsonList a -> compareListInt (firstElem,JsonInteger b)

compareIntList :: (Json,Json) -> BoolPlus
compateIntList (JsonInteger _, JsonList []) = Falsey
compareIntList (JsonInteger a, JsonList [b]) =
  case b of
    JsonList c -> compareIntList (JsonInteger a, JsonList [c !! 0])
    JsonInteger _ -> compareInts (JsonInteger a,b)
compareIntList (JsonInteger a, JsonList b)
  | length b == 0 = Falsey
  | otherwise = ans
  where
    firstElem = b !! 0
    ans = compare' (JsonInteger a, firstElem)
-- parser - do not touch
parseJsonMessage :: String -> Either String Json
parseJsonMessage a =
  let parseResult = runParser parseJson a
    in case parseResult of
      Right a -> Right $ fst a
      Left err -> Left err

newtype Parser a = Parser
  {
    runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap function (Parser x) =
    Parser (\input -> do
        (x',leftovers) <- x input
        return (function x', leftovers))

instance Applicative Parser where
  pure x = Parser (\input -> Right(x,input))
  (Parser p1) <*> (Parser p2) =
      Parser (\input -> do
          (x,leftovers) <- p1 input
          (x',leftovers') <- p2 leftovers
          return (x x',leftovers'))

instance {-# OVERLAPPING #-} Alternative (Either String) where
    empty = Left "empty"
    Left _ <|> b = b
    a <|> _ = a

instance Alternative Parser where
  empty = Parser Left
  (Parser p1) <|> (Parser p2) = Parser (\input -> p1 input <|> p2 input)

parseChar :: Char -> Parser Char
parseChar chr = Parser f
  where
    f (x:xs)
      | x == chr = Right (x,xs)
      | otherwise = Left ("Expected: " ++ show chr ++ "but got: " ++ show x)
    f [] = Left "Empty list"

takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP find = Parser (Right . span find)

notNull :: Parser [a] -> Parser [a]
notNull (Parser x) =
    Parser(\input -> do
             (parsed,leftovers) <- x input
             if null parsed
                then Left "Error: empty Integer"
                else Right(parsed, leftovers))

parseJsonInteger :: Parser Json
parseJsonInteger = JsonInteger . read <$> notNull (takeWhileP C.isDigit)

parseJsonList :: Parser Json
parseJsonList = fmap JsonList (parseChar '[' *> takeWhiteSpace *> splitByComma parseJson <* takeWhiteSpace <* parseChar ']')

splitByP :: Parser a -> Parser b -> Parser [b]
splitByP sep elements = (fmap (:) elements <*> many (sep *> elements)) <|> pure []

takeWhiteSpace :: Parser String
takeWhiteSpace = takeWhileP (==' ')

splitByComma :: Parser a -> Parser [a]
splitByComma = splitByP (takeWhiteSpace *> parseChar ',' <* takeWhiteSpace)

parseJson :: Parser Json
parseJson = parseJsonInteger <|> parseJsonList
