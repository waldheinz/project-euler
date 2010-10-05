
import Char

dsum :: Integer -> Int
dsum = sum . map digitToInt . show

main = print $ maximum $ map dsum [a^b | a <- [1..99], b <- [1..99]]
