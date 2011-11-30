
import Debug.Trace
import Data.List (groupBy, sort)

pand :: Int -> Int -> Int -> Bool
pand a b c = not ('0' `elem` s) && all (`elem` s) ['1'..'9'] && length s == 9 && length (groupBy (==) $ sort s) == 9 where
   s = show a ++ show b ++ show c

facts :: Int -> [(Int, Int)]
facts n = [(f1, f2) | f1 <- [2.. (1 + (floor $ sqrt $ fromIntegral n))], let f2 = n `div` f1, f1*f2 == n]

anyPand :: Int -> Bool
anyPand n = any (\(a, b, c) -> pand a b c) $ map (\(f1, f2) -> (f1, f2, n)) $ facts n

bound :: Int
bound = 9876

main :: IO ()
main = print $ sum $ map (\x -> trace (show x) x) $ filter anyPand [1..bound]
