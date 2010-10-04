
sumSq x = sum $ [y * y | y <- [1..x]]
sqSum x = s*s where
   s = sum [1..x]
   
main :: IO()
main = putStrLn $ show $ ((sqSum 100) - (sumSq 100))
