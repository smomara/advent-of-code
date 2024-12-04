import System.IO (readFile)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

type Point = (Int, Int)
type Grid = Map Point Char

makeGrid :: [String] -> Grid
makeGrid lines = M.fromList
    [ ((row, col), char)
    | (row, line) <- zip [0..] lines
    , (col, char) <- zip [0..] line ]

directions :: [Point]
directions = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

scale :: Int -> Point -> Point
scale i (dx, dy) = (i * dx, i * dy)

xmas :: Set Grid
xmas = S.fromList
    [M.fromList [(scale i d, x) | (i, x) <- zip [0..] "XMAS"] | d <- directions]

crossMas :: Set Grid
crossMas = S.fromList
    [ M.insert (0, 0) 'A' (diag1 `M.union` diag2)
    | diag1 <- M.fromList . zip [(-1,-1), (1,1)] <$> ["MS", "SM"]
    , diag2 <- M.fromList . zip [(1,-1), (-1,1)] <$> ["MS", "SM"] ]

matchesAt :: Grid -> Point -> Grid -> Bool
matchesAt grid (x, y) = all (checkpoint . M.toList)
  where checkPoint ((dx, dy), c) =  M.lookup (x + dx, x + dy) grid == Just char

countMatches :: Set Grid -> Grid -> Int
countMatches patterns grid = sum
    [ () | x <- [minX..maxX]
         , y <- [minY..maxY]
         , pattern <- S.toList patterns
         , matchesAt grid (x, y) pattern ]
  where
    points = M.keys grid
    [minX, maxX] = [minimum, maximum] <*> [map fst points]
    [minY, maxY] = [minimum, maximum] <*> [map snd points]

main :: IO ()
main = do
    input <- makeGrid . lines <$> readFile "input/day04.txt"
    print $ countMatches xmas input
    print $ countMatches crossMas input
