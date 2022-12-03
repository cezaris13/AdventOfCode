import Data.Either
import Data.List
import Data.Map ( (!), fromList, member )

main = do
  xs <- map (flip parse []) . lines <$> readFile "input.txt"
  let (corrupted, incomplete) = (lefts xs, rights xs)
  print $ sum . map score1 $ corrupted
  print $ (!! (length incomplete `div` 2)) . sort . map score2 $ incomplete

parse :: String -> String -> Either Char String
parse [] ys = Right ys
parse (x:xs) ys | x `member` parens = parse xs (parens ! x : ys)
  where parens = fromList . map (\[a, b] -> (a, b)) $ ["()", "[]", "{}", "<>"]
parse (x:xs) (y:ys) | x == y = parse xs ys
parse (x:xs) _ = Left x

score1 = (fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)] !)

score2 = foldl (\a b -> a * 5 + (scores ! b)) 0
  where scores = fromList [(')', 1), (']', 2), ('}', 3), ('>', 4)]
