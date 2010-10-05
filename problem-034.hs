
import Char

fac x = product [1..x]

valid :: Int -> Bool
valid x = (==x) . sum $ map (fac . digitToInt) (show x)

main = print $ sum $ filter valid [3..1000000]
