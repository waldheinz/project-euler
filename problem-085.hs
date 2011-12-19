
import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)

type Rect = (Int, Int)

subCount:: Rect -> Int
subCount (dx, dy) = sum $ map cnt subs where
   subs = [(x', y') | x' <- [1..dx], y' <- [1..dy]]
   cnt (x, y) = (dx - x  + 1) * (dy - y + 1)

main :: IO ()
main = do
   let
      u = head $ dropWhile (\x -> subCount (x, x) < 2000000) [1..]
      r = [(x, y) | x <- [1..u*2], y <- [x..u*2]]
      xs = zip r $ map subCount r
      xs' = sortBy (\(_, c1) (_, c2) -> comparing abs (2000000 - c1) (2000000 - c2)) xs
      x@((f1, f2), _) = head xs'
      
   print x
   print (f1 * f2)
   