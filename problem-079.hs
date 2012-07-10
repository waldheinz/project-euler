
import Data.Char (digitToInt)
import Data.List (tails, nub, intersect)
import Data.Graph (buildG, topSort)


main :: IO ()
main = do
	keys <- readFile "data/keylog.txt"
	let
	   edges = concatMap (\(a:b:c:[]) ->
			[(digitToInt a, digitToInt b), (digitToInt b, digitToInt c)]) $ lines keys
			
	   g = buildG (0, 9) edges
	   used = intersect ['0'..'9'] keys
	
	putStrLn $ "used chars : " ++ (show used)
	putStrLn $ "solution : " ++ (show $ intersect (concatMap show $ topSort g) used)
	putStrLn $ show edges
	putStrLn $ show $ nub edges
	putStrLn $ show $ concatMap show $ topSort g
   