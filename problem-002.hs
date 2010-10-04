
fibs :: [Int]
fibs = [1, 2] ++ fib 1 2 where
   fib a b = [a + b] ++ fib b (a + b)

main :: IO()
main = putStrLn $ show $ sum $ filter even $ takeWhile (<=4000000) fibs