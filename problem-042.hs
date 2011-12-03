
import Data.Char (ord)

parse :: String -> [String]
parse xs
   | null rest = [name]
   | otherwise = name : parse (tail rest)
   where
      name = takeWhile ((/=) '"') $ tail n
      (n, rest) = span ((/=) ',') xs

val :: String -> Int
val xs = sum $ map (\c -> ord c - ord 'A' + 1) xs
      
isTri :: Int -> Bool
isTri n = let n' = floor.sqrt.fromIntegral $ 2*n in 2*n == n' * (n' + 1)

main :: IO ()
main = do
   f <- readFile "words.txt"
   let
      i = map val $ parse f
      
   print $ length $ filter isTri $ i

