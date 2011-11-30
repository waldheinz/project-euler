
import Data.List (tails, sortBy)
import Data.Function (on)
import Debug.Trace

valid :: [String] -> String -> Bool
valid ks s = all (valid' s) ks where
   valid' :: String -> String -> Bool
   valid' [] cs = null cs
   valid' _ [] = True
   valid' s (c:cs)
      | null rest = False
      | otherwise = valid' (tail rest) cs
      where
         rest = dropWhile (/= c) s

shrink :: Int -> [String] -> String -> [String]
shrink _ _ [] = []
shrink sl ks all@(x:xs)
   | length all > sl = []
   | not $ valid ks all = []
   | valid ks xs = xs : rest
   | otherwise = rest
   where
      rest = concatMap (shrink sl' ks) $ map (x:) $ tail $ tails xs
      sl' = if valid ks xs then length xs else sl

main :: IO ()
main = do
   keys <- fmap lines $ readFile "keylog.txt"
   let
      start = concat keys
      x = shrink (length start) keys start
   print $ x
--    print $ all (valid keys) x
   print $ head $ sortBy (compare `on` length) x
