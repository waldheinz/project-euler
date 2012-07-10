
import Data.List (nub, maximumBy)
import Data.Ratio
import Data.Numbers.Primes

totient :: (Integral a) => a -> a
totient 1 = 1
totient n = numerator ratio `div` denominator ratio
 where ratio = foldl (\acc x -> acc * (1 - (1 % x))) 
                 (n % 1) $ nub (primeFactors n)

				 
withTot n = map (\x -> (x, fromIntegral x / fromIntegral (totient x))) [2..n]

maxTot n = maximumBy (\(_, t) (_, t2) -> compare t t2) $ withTot n

main = putStrLn $ show $ fst $ maxTot 1000000
