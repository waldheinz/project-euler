
import Data.Numbers.Primes

composites :: [Int]
composites = filter (\x -> odd x && (not . isPrime) x) [3..]

isSquare :: Int -> Bool
isSquare n = sq * sq == n where
	sq = floor $ sqrt $ (fromIntegral n::Double)

isPrimSq :: Int -> Bool
isPrimSq n = any (\p -> sqRest (n - p)) $ takeWhile (<n) primes where
	sqRest n'
		| odd n' = False
		| otherwise = isSquare (n' `div` 2)
		
main = putStrLn $ show $ filter (not . isPrimSq) composites
