
import Char (isSpace)

texts19 :: Int -> String
texts19 = (!!) ["zero", "one", "two", "three", "four", "five", "six", "seven",
   "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
   "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

texts99 :: Int -> String
texts99 n
   | n < 20 = texts19 n
   | n `mod` 10 == 0 = tens (n `div` 10 - 2)
   | otherwise = tens (n `div` 10 - 2) ++ "-" ++ texts19 (n `mod` 10)
   where
      tens = (!!) ["twenty", "thirty", "forty", "fifty", "sixty", "seventy",
         "eighty", "ninety"]

texts :: Int -> String
texts n
   | n < 100 = texts99 n
   | n == 1000 = "one thousand"
   | otherwise = texts19 (n `div` 100) ++ " hundred" ++ rest n
   where
      rest n
         | n `mod` 100 /= 0 = " and " ++ texts99 (n `mod` 100)
         | otherwise = ""

flt :: String -> String
flt = filter (\c -> ((not.isSpace) c) && c /= '-')

main :: IO ()
main = do
   let
      strs = concatMap texts [1..1000]
      strs' = flt strs
      
   print $ length strs'
   