
isPand :: String -> Bool
isPand s = (length s == 9) && and [x `elem` s | x <- ['1' .. '9']]

addDig :: Int -> Int -> String -> String
addDig base n str = str ++ show (n * base)

test base = filter isPand $ takeWhile ((<10).length) $ map snd $ iterate (\(n, s) -> (n+1, addDig base n s)) (1, "")

sol = concat $ map test [1..999999999]
