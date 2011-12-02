
import Data.List (foldl')

c :: Integer -> Integer -> Integer
c n k
   | k == 0 = 1
   | 2 * k > n = c n (n - k)
   | otherwise = foldl' (\e i -> (e * (n-k+i)) `div` i) (n-k+1) [2..k]
   
main :: IO ()
main = print.length.filter ((<) (10^6)) $ [c n k | n <- [1..100], k <- [0..n]]
