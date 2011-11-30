
poss :: Int -> [Int] -> Int
poss _ [] = 1 -- the rest is done with cent pieces
poss s (c:cs) = sum $ [poss (s - x) cs | x <- [0, c .. s]]

main :: IO ()
main = print $ poss 200 $ reverse [2, 5, 10, 20, 50, 100, 200]
