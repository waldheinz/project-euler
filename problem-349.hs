
import System.Console.ANSI
import Control.Monad (forM_)

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

nblack :: State -> Int
nblack = length . blacks

nblacks :: [(Int, Int)]
nblacks = zip [0..] $ map nblack $ iterate step initialState

drawc :: [Cell] -> String
drawc bs
   | null bs = "<empty>\n"
   | otherwise = concatMap row [y0..y1]
   where
      (x0, x1) = (minimum $ map fst bs ++ [-20], maximum $ map fst bs ++ [20])
      (y0, y1) = (minimum $ map snd bs ++ [-20], maximum $ map snd bs ++ [20])
      row y = [c x y | x <- [x0..x1]] ++ "\n"
      c x y = if (x, y) `elem` bs then '#' else '.'

draw :: State -> String
draw = drawc . blacks

main :: IO ()
main = do
   forM_ (zip [1..] (take 20000 $ iterate step initialState)) $ \(i, s) -> do
      setCursorPosition 0 0
      putStrLn $ draw s
      putStrLn $ show i
      
