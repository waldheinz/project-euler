
import Data.List
import Data.Ratio

refine :: Rational -> Rational -> Integer -> Integer -> [Rational]
refine lo hi n d
	| d == 1000000 = []
	| x >= hi = refine lo hi (n' - 1) (d + 1)
	| x <= lo = refine lo hi (n + 1) d
	| otherwise = xx ++ refine x hi n d
	where
		xx = [x] -- if gcd n d == 1 then [x] else []
		x = n % d
		n' = (numerator lo * (d + 1)) `div` denominator lo

main = print $ sort $ refine (2 % 5) (3 % 7) 3 7
