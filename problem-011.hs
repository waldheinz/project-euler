
import Data.Array
import Data.List (maximumBy)
import Data.Ord (comparing)

m = 20
len = 4

parse :: String -> [[Integer]]
parse t = map (\l -> map read $ words l) $ lines t

dirs :: [(Int, Int)]
dirs = [(1,0), (1,1), (0, 1), (-1, 1)]

starts :: [(Int, Int)]
starts = [(x, y) | x <- [0..m-1], y <- [0..m-1]]

path :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
path s d = p s d len where
   p _ _ 0 = []
   p (x, y) (dx, dy) l = (x, y) : p (x + dx, y + dy) (dx, dy) (l-1)
   
coords :: [[(Int, Int)]]
coords = concat $ map allDirs (map path starts) where
   allDirs f = map f dirs

fc = filter (all (\(x,y) -> x < m && y < m && x >= 0)) coords

eval :: (Array (Int, Int) Integer) -> [(Int, Int)] -> Integer
eval a p = product $ map (a!) p

arr l = listArray ((0,0), (19,19)) $ concat l :: Array (Int, Int) Integer

main = do
   input <- readFile "problem-011.txt"
   let ai = arr $ parse input
   let prods = map (eval ai) fc
   let zp = zip prods fc
   print $ maximumBy (comparing fst) zp
