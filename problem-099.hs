
import Data.List (maximumBy)

parse :: String -> [Integer]
parse s = map p $ lines s where
      p l = (\(b:e:[]) -> b ^ e) $ read $ '[' : l ++ [']']

main = do
     i <- readFile "data/base_exp.txt"
     print $ fst $ maximumBy (\(_, v1) (_, v2) -> compare v1 v2) $ zip [1..] (parse i)
