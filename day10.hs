{-# LANGUAGE RecordWildCards #-}

import Data.Char (digitToInt)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

type Point = (Int, Int)
type Path = [Point]
data Grid = Grid 
    { gridMap :: Map Point Int
    , rows :: Int
    , cols :: Int
    } deriving Show

makeGrid :: [String] -> Grid
makeGrid lines = Grid {..}
  where
    rows = length lines
    cols = length $ head lines
    gridMap = M.fromList
        [ ((r, c), digitToInt i)
        | (r, line) <- zip [0..] lines
        , (c, i) <- zip [0..] line
        ]

directions :: [(Int, Int)]
directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]

isValidStep :: Grid -> Point -> Point -> Bool
isValidStep Grid{..} p1 p2 = case (M.lookup p1 gridMap, M.lookup p2 gridMap) of
    (Just x, Just y) -> y == x + 1
    _ -> False

validNeighbors :: Grid -> Point -> [Point]
validNeighbors grid p@(x, y) =
    [ neighbor
    | neighbor <- [(x + dx, y + dy) | (dx, dy) <- directions]
    , isValidStep grid p neighbor
    ]

score :: Grid -> Point -> Int
score grid@Grid{..} start = bfs [start] S.empty S.empty
  where
    bfs :: [Point] -> Set Point -> Set Point -> Int
    bfs [] _ found9s = S.size found9s
    bfs (current:queue) visited found9s
        | M.lookup current gridMap == Just 9 =
            bfs queue visited (S.insert current found9s)
        | otherwise =
            let neighbors = validNeighbors grid current
                newPoints = [next | next <- neighbors, not (next `S.member` visited)]
                newVisited = S.insert current visited
            in bfs (queue ++ newPoints) newVisited found9s

rate :: Grid -> Point -> Int
rate grid@Grid{..} start = bfs [start] S.empty
  where
    bfs :: [Point] -> Set Point -> Int
    bfs [] _ = 0
    bfs (current:queue) visited
        | M.lookup current gridMap == Just 9 = 1 + bfs queue visited
        | otherwise =
            let neighbors = validNeighbors grid current
                newPoints = [next | next <- neighbors, not (next `S.member` visited)]
                newVisited = S.insert current visited
            in bfs (queue ++ newPoints) newVisited


main :: IO ()
main = do
    grid <- makeGrid . lines <$> readFile "input/day10.txt"
    print $ sum [score grid point | point <- M.keys (M.filter (==0) (gridMap grid))]
    print $ sum [rate grid point | point <- M.keys (M.filter (==0) (gridMap grid))]
