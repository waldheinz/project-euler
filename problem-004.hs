
palins = [a*b | a <- [100..999], b <-[a..999], show (a*b) == (reverse $ show (a*b))]

main :: IO()
main = putStrLn $ show $ maximum palins
