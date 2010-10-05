
import List

tris = scanl1 (+) [1..]
facs x = filter (\d -> x `mod` d == 0) [1..(x `div` 2)] ++ [x]
res = head $ dropWhile ((< 500) . nDiv) tris
res1 = [(x, nDiv x) | x <- tris]

primes = 2 : filter ((==1) . length . primeFactors) [3,5..]

primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      = factor n ps

nDiv n = product $ map ((+1) . length) (group (primeFactors n))

main = print res