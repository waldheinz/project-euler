
digits :: Int -> [Int]
digits n = map (\c -> read [c]) $ show n

psum :: Int -> Bool
psum n = n == (sum $ map (^ 5) $ digits n)

main :: IO ()
main = print $ sum $ tail $ filter psum [1..200000]
