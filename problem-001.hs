
sumBelow :: Int -> Int
sumBelow max = foldl maybeAdd 0 [1..(max-1)] where
   maybeAdd s x
      | mod x 3 == 0 = x + s
      | mod x 5 == 0 = x + s
      | otherwise = s

main :: IO()
main = putStrLn $ show (sumBelow 1000)