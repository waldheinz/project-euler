
import Data.Maybe (mapMaybe, isNothing, fromJust)

properSimpl :: (Int, Int) -> (Int, Int)
properSimpl (x, y) = (x `div` g, y `div` g) where
   g = gcd x y

simpl :: (Int, Int) -> Maybe (Int, Int)
simpl x
   | isNothing ms = Nothing
   | properSimpl (fromJust ms) == properSimpl x = Just $ fromJust ms
   | otherwise = Nothing
   where
      ms = simpl' x

simpl' :: (Int, Int) -> Maybe (Int, Int)
simpl' (x, y)
   | head sx /= '0' && head sx == head sy = Just (read $ tail sx, read $ tail sy)
   | head sx' == head sy = Just (read $ tail sx', read $ tail sy)
   | otherwise = Nothing
   where
      (sx, sy) = (show x, show y)
      (sx', sy') = (reverse sx, reverse sy)



main :: IO ()
main = do
   let
      sols = mapMaybe simpl [(x, y) | x <- [10..99], y <- [x+1..99]]
      prod = foldl (\(a,b) (c,d) -> (a*c, b*d)) (1, 1) sols
      
   print $ properSimpl prod
