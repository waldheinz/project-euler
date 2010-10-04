
valid :: Int -> Bool
valid x = all (==0) $ map (mod x) [1..20]

main :: IO()
main = putStrLn $ show $ head $ filter valid [1..]
