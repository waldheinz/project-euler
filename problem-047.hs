
import Data.List (nub, isPrefixOf)
import Data.Numbers.Primes

withPfLen :: [(Int, Int)]
withPfLen = map (\x -> (x, length $ nub $ primeFactors x)) [1..]

rightPfCount :: [Int]
rightPfCount = map fst $ filter (\(_, n) -> n == 4) withPfLen

filtered :: [Int] -> [Int]
filtered [] = []
filtered (x:xs)
	| isPrefixOf [(x+1)..(x+3)] xs = x : filtered xs
	| otherwise = filtered xs
	