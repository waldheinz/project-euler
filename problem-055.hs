
-- would be sufficient to check half the digits, but so what...
palin :: Integer -> Bool
palin n = let s = show n in all (uncurry (==)) $ zip s $ reverse s

step :: Integer -> Integer
step n = n + (read.reverse.show) n

nlychrel :: Integer -> Bool
nlychrel n = any palin $ take 50 $ tail $ iterate step n

main :: IO ()
main = print $ fromIntegral n - (length $ filter nlychrel [1..n]) where
   n = 9999
