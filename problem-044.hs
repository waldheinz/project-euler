

isPent n = (af == 0) && ai `mod` 6 == 5 where
	(ai, af) = properFraction . sqrt $ 1 + 24 * (fromIntegral n)

pent :: Integer -> Integer
pent n = n * (3 * n - 1) `div` 2

pentPairs :: [(Integer, Integer)]
pentPairs = [(pent i, pent j) | i <- [1..5000], j <- [i..5000], i /= j]

accept :: (Integer, Integer) -> Bool
accept (p1, p2) = isPent (p1 + p2) && isPent (p2 - p1)

main = putStrLn $ show $ head $ map (\(a, b) -> b - a) $ filter accept pentPairs
