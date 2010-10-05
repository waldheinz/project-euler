import Char
import Numeric

toBin x = showIntAtBase 2 (\c -> chr $ c + ord '0') x []

pal2 x = (toBin x) == (reverse $ toBin x)
pal10 x = (show x) == (reverse $ show x)

main = print $ sum $ filter (\d -> pal2 d && pal10 d) [1..999999]