
import Data.Char (digitToInt)

next :: Int -> Int
next n = sum $ map ((\x -> x*x).digitToInt) $ show n

chainEnd :: Int -> Int
chainEnd n = head $ dropWhile (\x -> x /= 1 && x /= 89) $ iterate next n

main :: IO ()
main = print $ length $ filter ((==)89) $ map chainEnd [1..9999999]
