
import Data.List
import Data.Ratio

refine :: Rational -> Rational -> Integer -> Integer -> [Rational]
refine lo hi n d
	| d >12000 = []
	| x >= hi = refine lo hi n' (d + 1)
	| x <= lo = refine lo hi (n + 1) d
	| otherwise = xx ++ refine lo hi (n+1) d
	where
		xx = if gcd n d == 1 then [x] else []
		x = n % d
		n' = (numerator lo * (d + 1)) `div` denominator lo

main = print $ length $ refine (1 % 3) (1 % 2) 1 3
