
 
main = putStrLn $ show $ head [x | x <- scanl (+) 1 [5,9..], x > 40755, isPent x] where
	isPent n = (af == 0) && ai `mod` 6 == 5 where
		(ai, af) = properFraction . sqrt $ 1 + 24 * (fromInteger n)
