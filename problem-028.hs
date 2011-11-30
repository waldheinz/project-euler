
main :: IO ()
main = print $ sum $ map diag [1, 3 .. 1001] where
   diag 1 = 1
   diag n = sum $ take 4 [n*n, n*n - (n-1) ..]
