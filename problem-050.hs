
import Data.List (inits, tails, sortBy)
import Data.Numbers.Primes

consecs = filter ((>10) . length) $ concatMap tails ((drop 2 . inits) $ takeWhile (< 10000) primes)

withSum :: [(Integer, Int)]
withSum = map (\x -> (sum x, length x)) consecs

primeSums = filter (\(s, _) -> (s < 1000000) && isPrime s) withSum

sorted = sortBy(\(_, l1) (_, l2) -> compare l2 l1) primeSums

main = putStrLn $ show $ head sorted
