
import Data.Function (on)
import Data.List (maximumBy)

tris :: Int -> [(Int, Int, Int)]
tris p = [(a, b, c) | a <- [1..p], b <- [a..p], let c = p - (a + b), c > 0, (a*a) + (b*b) == (c*c)]

main :: IO ()
main = do
   let
      ns = [1..1000]
      x = zip ns $ map tris ns
      
   print $ maximumBy (compare `on` (length.snd)) x