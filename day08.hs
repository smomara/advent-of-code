{-# LANGUAGE RecordWildCards #-}

import System.IO (readFile)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

type Freq = Char
type Point = (Int, Int)
type Coords = Set Point

data Grid = Grid
    { antennas :: Map Freq Coords
    , bounds :: (Int, Int) -- (rows, cols)
    }

mkGrid :: [String] -> Grid
mkGrid lines = Grid {..}
  where
    bounds = (length lines, length $ head lines)
    antennas = M.fromListWith S.union
        [ (char, S.singleton (row, col))
        | (row, line) <- zip [0..] lines
        , (col, char) <- zip [0..] line
        , char /= '.'
        ]

antinodes :: Set Point -> Set Point
antinodes points = S.fromList
    [ (x2 + (x2- x1), y2 + (y2 - y1))
    | p1@(x1, y1) <- S.toList points
    , p2@(x2, y2) <- S.toList points
    , p1 /= p2
    ]

antinodes' :: (Int, Int) -> Set Point -> Set Point
antinodes' (rows, cols) points = S.fromList
    [ (x2 + n * (x2- x1), y2 + n * (y2 - y1))
    | p1@(x1, y1) <- S.toList points
    , p2@(x2, y2) <- S.toList points
    , p1 /= p2
    , let dx = x2 - x1
    , let dy = y2 - y1
    , n <- [1..maxSteps dx dy x2 y2]
    ]
    where
        maxSteps dx dy x2 y2 =
            let stepsToEdgeX = if dx == 0 then maxBound
                               else if dx > 0 then (rows - 1 - x2) `div` dx
                               else (-x2) `div` dx
                stepsToEdgeY = if dy == 0 then maxBound
                               else if dy > 0 then (cols - 1 - y2) `div` dy
                               else (-y2) `div` dy
            in min stepsToEdgeX stepsToEdgeY

processGrid :: Grid -> Coords
processGrid Grid{..} = 
    S.filter inBounds $ M.foldr (S.union . antinodes) S.empty antennas
  where
    inBounds (y, x) =
        let (rows, cols) = bounds
        in 0 <= y && y < cols && 0 <= x && x < rows

processGrid' :: Grid -> Coords
processGrid' Grid{..} = 
    S.filter inBounds $ M.foldr addPoints S.empty antennas
  where
    addPoints points acc = S.union points $ S.union (antinodes' bounds points) acc
    inBounds (y, x) =
        let (rows, cols) = bounds
        in 0 <= y && y < cols && 0 <= x && x < rows

main :: IO ()
main = do
    input <- mkGrid . lines <$> readFile "input/day08.txt"
    print . S.size . processGrid $ input
    print . S.size . processGrid' $ input
