import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

type Point = (Int, Int)
type Grid = Map Char (Set Point)

makeGrid :: [String] -> Grid
makeGrid lines = M.fromListWith S.union
    [ (char, S.singleton (row, col))
    | (row, line) <- zip [0..] lines
    , (col, char) <- zip [0..] line
    ]

directions :: [(Int, Int)] -- cardinal directions
directions = [(1,0), (-1,0), (0,1), (0,-1)]

neighbors :: Point -> [Point]
neighbors (x, y) = [(x+dx, y+dy) | (dx, dy) <- directions]

getRegion :: Set Point -> Point -> Set Point
getRegion points startPoint = go S.empty startPoint
  where
    go :: Set Point -> Point -> Set Point
    go visited current
      | current `S.notMember` points = visited
      | current `S.member` visited = visited
      | otherwise = foldr (flip go) newVisited (neighbors current)
      where
        newVisited = S.insert current visited

getRegions :: Grid -> [[Point]]
getRegions grid = concatMap splitIntoRegions (M.toList grid)
  where
    splitIntoRegions (_, points) = map S.toList $ go points
    go points
      | S.null points = []
      | otherwise = region : go remaining
      where
        start = head $ S.toList points
        region = getRegion points start
        remaining = points `S.difference` region

calcPerimeter :: Set Point -> Int
calcPerimeter points = sum 
    [4 - length (filter (`S.member` points) (neighbors p)) | p <- S.toList points]

countSides :: Set Point -> Int
countSides points = undefined

main :: IO ()
main = do
    grid <- makeGrid . lines <$> readFile "input/day12_test.txt"
    let regions = getRegions grid
    print $ sum [length region * calcPerimeter (S.fromList region) | region <- regions]
    print $ sum [length region * countSides (S.fromList region) | region <- regions]
