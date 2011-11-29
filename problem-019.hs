
type Date = (Int, Int, Int) -- day, month, year

leapYear :: Int -> Bool
leapYear y
   | y `mod` 4 /= 0 = False
   | y `mod` 400 == 0 = True
   | y `mod` 100 == 0 = False
   | otherwise = True

monthDays :: Int -> Int -> Int
monthDays m y
   | m == 2 = if leapYear y then 29 else 28
   | m `elem` [4, 6, 9, 11] = 30
   | otherwise = 31

next :: Date -> Date
next (d, m, y)
   | d == monthDays m y = nextMonth
   | otherwise = (d+1, m, y)
   where
      nextMonth
         | m == 12 = (1, 1, y+1)
         | otherwise = (1, m+1, y)

match :: (Int, Date) -> Bool
match (7, (1, _ , _)) = True
match _ = False

main :: IO ()
main = do
   let
      days = cycle [1, 2, 3, 4, 5, 6, 7] :: [Int]
      dates = takeWhile (\(_, _, y) -> y < 2001) $ iterate next (1,1,1900)
      result = filter match $ zip days dates
      result' = filter (\(_, (_, _, y)) -> y > 1900) result
      
   print $ map snd result'
   print $ length result'
   
   