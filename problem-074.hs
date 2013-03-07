
import Data.Array
import Data.Char (ord)


facts = listArray (ord '0', ord '9') $ scanl (*) 1 [1..9]

-- next :: Integer -> Integer
next n = sum $ map (\d -> facts ! (ord d)) (show n)
   
-- allFs :: Integer -> Integer
allFs n = (go [n]) where
   go xs@(x:_)
      | nx `elem` xs = fromIntegral (length xs)
      | otherwise = go (nx:xs)
      where
         nx = next x
         
chains :: [Int]
chains = filter (==60) $ map allFs [1..999999]

main = putStrLn $ show $ length chains
   
