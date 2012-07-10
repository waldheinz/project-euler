

addDigit :: Int -> (Int, Int) -> Integer
addDigit n (d1, d2)
	 | n == 1 = fromIntegral (9 - s + 1)
         | otherwise = sum $ map (\x -> addDigit (n-1) (d2, x)) [0..(9 - s)]
         where
                s = d1 + d2

lupSize = 10 :: Int

lup :: [[Integer]]
lup = [[addDigit lupSize (a, b) | b <- [0..9-a]] | a <- [0..9]]

addDigit' :: Int -> (Int, Int) -> Integer
addDigit' n (d1, d2)
         | n == lupSize = lup !! d1 !! d2
         | otherwise = sum $ map (\x -> addDigit' (n-1) (d2, x)) [0..(9 - s)]
         where
                s = d1 + d2

sol :: [Integer]
sol = [addDigit' 18 (b, c) | b <- [1..9], c <- [0..9-b]]

main = do
     let s = sol
     mapM_ (putStrLn . show) s
     putStrLn $ " sum = " ++ (show $ sum s)
