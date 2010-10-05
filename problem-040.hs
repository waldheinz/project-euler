
import Data.Char

nums = concat $ map show [1..]
d = digitToInt . (nums !!)
res = map (\x -> d (x-1)) [1, 10, 100, 1000, 10000, 100000, 1000000]
main = print $ product res
