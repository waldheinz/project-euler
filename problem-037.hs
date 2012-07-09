
import Data.List (inits, tails, nub)
import Data.Numbers.Primes (primes, isPrime)

trunc :: Integer -> [Integer]
trunc n = nub $ left ++ right where
	left = map read $ tail $ init $ tails $ show n
	right = map read $ tail $ init $ inits $ show n

sol = take 11 $ filter (\x -> all isPrime $ trunc x) $ dropWhile (<8) primes
	
main :: IO ()
main = do
	putStrLn $ show (sum sol)
