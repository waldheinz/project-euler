
import Control.Monad (forM_)
import Data.List (genericLength)

primes :: [Integer]
primes = sieve [2..] where
   sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]  

-- (eq. to) find2km (2^k * n) = (k,n)
find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        
 
-- n is the number to test; a is the (presumably randomly chosen) witness
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = 
        error $ "millerRabinPrimality: a out of range (" 
              ++ show a ++ " for "++ show n ++ ")" 
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs
 
-- (eq. to) pow' (*) (^2) n k = n^k
pow' :: (Num a, Integral b) => (a->a->a) -> (a->a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a
 
-- (eq. to) powMod m n k = n^k `mod` m
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

isPrime :: Integer -> Bool
isPrime n
        | n == 1 = False
        | n == 3 = True
        | otherwise = and $ map (millerRabinPrimality n) [2, 3]

edges :: Integer -> [Integer]
edges sl = take 4 $ iterate (+(sl-1)) $ 1 + (sl-2) * (sl-1)

cntPrim :: [Integer] -> [Integer] -> (Integer, [Integer])
cntPrim prims cands = go cands (0, prims) where
        go [] a = a
        go xx@(x:xs) (r, p:ps)
           | x == p = go xs (r + 1, ps)
           | x < p = go xs (r, p:ps)
           | otherwise = go xx (r, ps)

chain :: [(Integer, Integer)] -- layers, prim count
--chain = map (\(l, pc, _) -> (l, pc)) $ iterate
--      (\(l, c, ps) -> let (c', ps') = cntPrim ps $ edges (l+2) in (l+2, c + c', ps')) (1, 0, primes)

chain = go 0 chain' where
      go s ((l, pc) : xs) = (l, s+pc) : go (s + pc) xs 
      chain' = map (\l -> (l, genericLength $ filter isPrime $ edges l)) [1, 3..]

appr :: [(Integer, Integer, Bool)]
appr = map (\(l, pc) -> (l, pc, pc * 10 < ((l-1) * 2 + 1))) chain

main = forM_ appr $ \(l, pc, good) -> do
     putStrLn $ (show l)  ++ " r=" ++ (show (fromIntegral pc / fromIntegral ((l-1) * 2 + 1))) 
     if good && l > 1
        then error "out"
        else return ()