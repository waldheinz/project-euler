
import Data.Bits
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

fitness :: String -> Int
fitness [] = 0
fitness (c:cs)
   | c == ' ' = 1 + fitness cs
   | otherwise = fitness cs

comp :: (String, Int) -> (String, Int) -> (String, Int)
comp a@(ma, fa) b@(mb, fb)
   | fa > fb = a
   | otherwise = b

charSum :: String -> Int
charSum [] = 0
charSum (c:cs) = (ord c) + charSum cs

crack :: String -> String
crack i = show $ charSum $ fst best where
   best = foldl comp ("", 0) fits
   fits = zip decs (map fitness decs)
   decs = map (decode enc) passes
   enc =  map read $ splitBy (==',') i :: [Int32]
   
main = do
   c <- readFile "problem-059.txt"
   print $ crack c
   