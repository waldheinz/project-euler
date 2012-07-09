
type Point = (Integer, Integer)

sub :: Point -> Point -> Point
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

--dot :: Point -> Point -> Integer
--dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2 + z1 * z2

cross :: Point -> Point -> Integer
cross (a1, a2) (b1, b2) = a1 * b2 - a2 * b1

-- | do the first two points lie in the same half-space determined by the latter two points?
sameSide :: Point -> Point -> Point -> Point -> Bool
sameSide p1 p2 a b = d1 * d2 > 0 where
	d1 = (b `sub` a) `cross` (p1 `sub` a)
	d2 = (b `sub` a) `cross` (p2 `sub` a)

type Tri = (Point, Point, Point)

hasPt :: Point -> Tri -> Bool
hasPt p (a, b, c) = and [sameSide p a b c, sameSide p b a c, sameSide p c a b]

split :: String -> Char -> [String]
split [] delim = [""]
split (c:cs) delim
	| c == delim = "" : rest
	| otherwise = (c : head rest) : tail rest
	where
		rest = split cs delim

pairs :: [String] -> [(String, String)]
pairs [] = []
pairs (a:b:rest) = (a, b) : pairs rest
		
parse :: String -> [Tri]
parse s = map (\(a:b:c:[]) -> (a, b, c)) $ map pts $ lines s where
	pts l = map (\(sx, sy) -> (read sx, read sy)) $ pairs $ split l ','
	
main :: IO ()
main = do
	inp <- readFile "data/triangles.txt"
	putStrLn $ show $ length $ filter (hasPt (0, 0)) $ parse inp
	
	   