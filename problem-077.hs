

-- | Turner's sieve
primes :: [Integer]
primes = sieve [2..] where
   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]  

primeSum :: Integer -> [Integer] -> Integer
primeSum t pp@(p:ps)
	| t < p = 0
	| t == p = 1
	| otherwise = primeSum (t - p) pp + primeSum t ps
	
main = putStrLn $ show $ head $ dropWhile (\t -> primeSum t primes <= 5000) [1..]