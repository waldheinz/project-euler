
import Data.Char (digitToInt)
import Data.List (group, tails)
import Debug.Trace

bouncy :: Int -> Bool
bouncy n
   | n < 100 = False
   | length ns < 2 = False
   | d1 < d2 = not $ all (cmp (<=)) $ tails (d2:ds)
   | d1 > d2 = not $ all (cmp (>=)) $ tails (d2:ds)
   where
      cmp f xs = case xs of
                    (a:b:_) -> f a b
                    _ -> True
                    
      ns = map (digitToInt.head) $ group $ show n
      (d1:d2:ds) = ns


properSimpl :: (Int, Int) -> (Int, Int)
properSimpl (x, y) = (x `div` g, y `div` g) where
   g = gcd x y

main :: IO ()
main = do
   let
      ns = map bouncy [1..]
      ns' = tail $ scanl (\(nb, nt) b -> if b then (nb+1, nt+1) else (nb, nt+1)) (0, 0) ns
--   print $ map properSimpl ns'
   print $ head $ dropWhile (\x -> properSimpl x /= (99,100)) $ ns'
