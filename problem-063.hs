
gen :: Integer -> [Integer]
gen e = filter (\v -> e == (fromIntegral.length.show) v) ns where
   ns = takeWhile (\v -> (fromIntegral.length.show) v <= e) $  map (\x -> x ^ e) [1..]

main :: IO ()
main = print $ length $ concatMap gen [1..100]
