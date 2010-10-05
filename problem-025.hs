
fibs = tail fibs' where
   fibs' = 1 : 1 : (scanl1 (+) fibs')

nfibs = zip [1..] fibs

main = print $ head $ dropWhile ((<1000) . length . show . snd) nfibs