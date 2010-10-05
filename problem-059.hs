
import Bits
import Int
import Char

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
   (first, rest) = break f list

cands :: [String]
cands = [[a,b,c] | a <- ['a' .. 'z'], b <- ['a' .. 'z'], c <- ['a' .. 'z']]

--passes :: [Int32]
passes = map (cycle . (map (fromIntegral . ord))) cands

--decode :: [Int32] -> [Int32] -> String
decode m p = map chr $ map fromIntegral $ zipWith xor m p

crack :: String -> String
crack i = show ints where
   ints =  map read $ splitBy (==',') i :: [Int32]
   
main = do
   c <- readFile "problem-059.txt"
   print $ crack c
   