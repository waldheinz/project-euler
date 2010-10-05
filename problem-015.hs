import Data.List

-- works, but won't finish anytime soon
paths = nub $ permutations ((take 20 $ repeat 'r') ++ (take 20 $ repeat 'd'))
result = length $ paths

fac n = product [1..n]
routes n = (fac (2 * n)) `div` ((fac n) * (fac n))

main = print $ routes 20
