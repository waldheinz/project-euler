
import Data.Char (ord)
import Data.List (foldl', sort)

parse :: String -> [String]
parse xs
   | null rest = [name]
   | otherwise = name : parse (tail rest)
   where
      name = takeWhile ((/=) '"') $ tail n
      (n, rest) = span ((/=) ',') xs

aval :: (Int, String) -> Int
aval (p, n) = p * foldl' (\s c -> ord c - ord 'A' + 1 + s) 0 n

main :: IO ()
main = do
   f <- readFile "names.txt"
   print $ sum $ map aval $ zip [1..] $ sort $ parse f
   