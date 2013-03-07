
matches :: Integer -> Bool
matches n
   | length sn /= 19 = False
   | otherwise =
      ((sn !! 0) == '1') &&
      ((sn !! 2) == '2') &&
      ((sn !! 4) == '3') &&
      ((sn !! 6) == '4') &&
      ((sn !! 8) == '5') &&
      ((sn !! 10) == '6') &&
      ((sn !! 12) == '7') &&
      ((sn !! 14) == '8') &&
      ((sn !! 16) == '9') &&
      ((sn !! 18) == '0')
   where
      sn = show (n*n)
      
main = putStrLn $ show $ head $ dropWhile (not . matches) [10^9..]

