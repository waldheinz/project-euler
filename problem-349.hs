
import System.Console.ANSI
import Control.Monad (forM_)
import Data.Function
import Data.List

data Dir = North | East | South | West deriving (Enum, Eq, Show)

type Cell = (Int, Int)

data State = State
   { pos    :: ! Cell
   , blacks :: [Cell]
   , dir    :: ! Dir
   } deriving (Show)

initialState :: State
initialState = State (0, 0) [] North

move :: Dir -> Cell -> Cell
move North  (x, y) = (x, y-1)
move East   (x, y) = (x+1, y)
move South  (x, y) = (x, y+1)
move West   (x, y) = (x-1, y)

rotate :: Bool -> Dir -> Dir
rotate False d = if d == West then North else succ d
rotate True d = if d == North then West else pred d

step :: State -> State
step s
   | isBlack = State p' (b1 ++ tail b2) d'
   | otherwise = State p' (pos s : b1) d'
   where
      d' = rotate isBlack (dir s)
      p' = move d' (pos s)
      (b1, b2) = span (/= (pos s)) (blacks s)
      isBlack = not $ null b2


-- just for the fun of it, an ASCII - art animation

draw :: State -> String
draw (State _ bs _)
   | null bs = "<empty>\n"
   | otherwise = concatMap row [y0..y1]
   where
      (x0, x1) = (minimum $ map fst bs ++ [-10], maximum $ map fst bs ++ [10])
      (y0, y1) = (minimum $ map snd bs ++ [-10], maximum $ map snd bs ++ [10])
      row y = [c x y | x <- [x0..x1]] ++ "\n"
      c x y = if (x, y) `elem` bs then '#' else '.'

animate :: IO ()
animate = forM_ (zip [1..] (iterate step initialState)) $ \(i, s) -> do
   setCursorPosition 0 0
   putStrLn $ draw s
   putStrLn $ show i

-- and the real thing...

nblacks :: [(Int, Int)]
nblacks =  drop 11000 $ zip [0..] $ map (length.blacks) $ iterate step initialState

flatten :: (Int, Int) -> (Int, Int)
flatten (n, b) = (n, 3 * n - 26 * b - 11169) -- found by playing with gnuplot

main :: IO ()
main = do
   let
      n = 11001 -- 10 ^ 18
      sample = take 1000 nblacks
      lin = map flatten sample
      min = minimumBy (compare `on` snd) lin
      mins = filter (\(_, b) -> b == snd min) lin
      plen = (\((n1, _) : (n2, _) : _) -> n2 - n1) mins -- period length
      (offset, target) = ((n  - 11000) `mod` (fromIntegral plen :: Integer),
                          (n  - 11000) `div` (fromIntegral plen :: Integer))
      sample' = take plen $ map (fromIntegral.snd) $ dropWhile ((/=) min) lin
      
   print $ "minimum = " ++ show min
   print $ "mins = " ++ show mins
   print $ "plen = " ++ show plen
   print $ "target, offset = (" ++ show target ++ ", "  ++ show offset ++ ")"
   print $ sample'
   print $ ((11169 + target) * 3 + (sample' !! fromIntegral offset)) `div` 26
   print $ nblacks !! 1