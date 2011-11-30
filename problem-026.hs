
import Data.List (elemIndex, maximumBy)

oneDiv :: Int -> [Int]
oneDiv = go (1, [1]) where
   go (o, os) u
      | o' `elem` os = os ++ [o']
      | o == 0 = os
      | o == u = os
      | otherwise = go (o', os ++ [o']) u
      where
         o' = (o * 10) `mod` u

clen :: [Int] -> Int
clen xs = maybe 0 (+1) $ elemIndex (head xs') $ tail xs' where
   xs' = reverse xs

main :: IO ()
main = let
           ls = [(clen $ oneDiv x, x) | x <- [1..999]]
           m = maximumBy (\(l1, _) (l2, _) -> compare l1 l2) ls
       in print m