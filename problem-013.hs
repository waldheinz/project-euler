
main = do
   ls <- fmap (map read . lines) (readFile "problem-013.txt")
   print $ take 10 $ show $ sum ls