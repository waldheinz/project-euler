
import Debug.Trace

-- | Turner's sieve
primes :: [Integer]
primes = sieve [2..] where
   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]  

largestPf :: Integer -> Integer
largestPf x = f primes x 0 where
   f (p:ps) x lrg
      | p == x = p
      | x `mod` p == 0 = f ps (x `div` p) p
      | p >= x = lrg
      | otherwise = f ps x lrg

main :: IO()
main = putStrLn $ show $ largestPf 600851475143
