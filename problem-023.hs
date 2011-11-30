
import Debug.Trace

divs :: Int -> [Int]
divs x = filter (\d -> x `rem` d == 0) [1..(x `div` 2)]

abun :: Int -> Bool
abun x = (> x) . sum $ divs x

abunds :: [Int]
abunds = takeWhile (< 15000) $ filter abun [1..]

abunp :: Int -> [(Int, Int)]
abunp x = [(a, b) | a <- takeWhile (<= x `div` 2) abunds, let b = x-a, abun b]

main = print $ sum $ filter (\x -> trace (show x) $ (null . abunp) x) [1..28123]
