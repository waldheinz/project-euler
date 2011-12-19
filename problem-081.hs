
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
type Board = [[Int]]

testBoard :: Board
testBoard = [
   [131, 673, 234, 103,  18],
   [201,  96, 342, 965, 150],
   [630, 803, 746, 422, 111],
   [537, 699, 497, 121, 956],
   [805, 732, 524,  37, 331]
   ]

cost :: Board -> Int
cost [] = 0
cost [[]] = 0
cost ((x:xs):[]) = x + sum xs -- walk straight right
cost ((x:[]):xs) = x + (sum $ map head xs) -- walk straight down
cost b@((x:_):_) = x + min (cost $ map tail b) (cost $ tail b)

parse :: String -> Board
parse s = map (\l -> map read $ wordsWhen (==',') l) $ lines s

main :: IO ()
main = do
   i <- readFile "matrix.txt"

   print $ cost $ parse i
