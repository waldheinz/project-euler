
import Data.List (sort, groupBy)

isDivBy :: Int -> Int -> Int -> [Int]
isDivBy d1 d2 di = [x | x <- [0..9], (n + x) `rem` di == 0] where
   n = 100 * d1 + 10 * d2

cands :: [[Int]]
cands = [d1:d2:d3:d4:d5:d6:d7:d8:d9:d10:[] |
         d1 <- [0..9],
         d2 <- [0..9],
         d3 <- [0..9],
         d4 <- isDivBy d2 d3 2,
         d5 <- isDivBy d3 d4 3,
         d6 <- isDivBy d4 d5 5,
         d7 <- isDivBy d5 d6 7,
         d8 <- isDivBy d6 d7 11,
         d9 <- isDivBy d7 d8 13,
         d10 <- isDivBy d8 d9 17 ]

pand :: [Int] -> Bool
pand s = all (`elem` s) [0..9] && length s == 10 && length (groupBy (==) $ sort s) == 10

main :: IO ()
main = print $ sum $ map read $ map (foldl (\s i -> s ++ show i) "") $ filter pand $ cands

