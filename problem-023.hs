
divs x = filter (\d -> x `mod` d == 0) [1..(x `div` 2)]
abun x = (> x) . sum $ divs x
abunds = filter abun [1..]
abunp x = [(a, b) | a <- takeWhile (< x `div` 2) abunds, let b = x-a, abun b]
sabun = not . null . abunp

main = print $ sum $ filter (not . sabun) [1..28123]
