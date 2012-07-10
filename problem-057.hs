

type Frac = (Integer, Integer)

simpl :: Frac -> Frac
simpl (d, n) = let x = gcd d n in (d `div` x, n `div` x)

fadd :: Frac -> Frac -> Frac
fadd (a1, b1) (a2, b2) = simpl (a1 * b2 + a2 * b1, b1 * b2)

finv :: Frac -> Frac
finv (a, b) = (b, a)

expand :: Int -> Frac
expand n
	| n == 0 = (1, 2)
	| otherwise = finv (fadd (2, 1) $ expand (n-1))

iter :: Int -> Frac
iter n = fadd (1, 1) $ expand n

match :: Frac -> Bool
match (n, d) = length (show n) > length (show d)

main = do
	let result = filter match $ map iter [1..1000]
	mapM_ (putStrLn . show) result
	putStrLn $ "count = " ++ (show $ length result)
	