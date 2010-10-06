
readTri :: String -> [[Int]]
readTri s = map (\l -> map read $ words l) $ lines s

--paths :: Int -> Int -> [[Int]] -> [Int]
--paths p c [] = c
--paths p c (l:ls) = (l !! p) + 

p18 = foldr1 g where
   f x y z = x + max y z
   g xs ys = zipWith3 f xs ys $ tail ys

main = do
   input <- readFile "problem-067.txt"
   print $ head $ p18 $ readTri input
