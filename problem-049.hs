
import Data.List (sort)
import Data.Numbers.Primes

seqs :: [[Int]]
seqs = [take 3 $ iterate (+inc) start | start <- [1000..9999], inc <- [2..(9999 - start) `div` 2]]

isPerm :: [Int] -> Bool
isPerm (x:xs) = all (\t -> sort (show t) == sort (show x)) xs

main = mapM_ (putStrLn . show) $ filter (\x -> isPerm x && (all isPrime x)) seqs
