 
import Debug.Trace

isBlack :: Integer -> Integer -> Integer -> Bool
isBlack n x y = ((x - (2 ^ (n-1))) ^ 2) +
                ((y - (2 ^ (n-1))) ^ 2) <= 2 ^ (2 * n - 2)

draw :: Integer -> [String]
draw n = map line [0..m] where
     m = (2 ^ n) - 1
     line y = map (pt y) [0..m]
     pt y x
        | isBlack n x y = '#'
        | otherwise = ' '

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = and $ map (== head xs) (tail xs)

encode :: Integer -> Integer -> Integer -> Integer -> Integer
encode n x y s
   --    | trace ( (show x) ++ " " ++ (show y) ++ " " ++ (show s) ) False
       | s == 0 || solid = 2 -- True : [ if isBlack n x y then True else False ]
       | otherwise = 1 + sum [ encode n x' y' (s-1) |
                                      x' <- [x , x + (2 ^ (s-1))],
                                      y' <- [y , y + (2 ^ (s-1))]]
       where
        solid
                | n == s = False -- force first split
                | otherwise = allTheSame [isBlack n x' y' | 
                                                   x' <- [x , x + (2 ^ s) - 1],
                                                   y' <- [y , y + (2 ^ s) - 1]]

main :: IO ()
main = do
     let n = 24
     putStrLn "--"
   --  mapM_ putStrLn $ draw n
     putStrLn "--"
     putStrLn $ show $ encode n 0 0 n
     