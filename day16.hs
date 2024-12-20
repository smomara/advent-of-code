{-# LANGUAGE RecordWildCards #-}

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.PQueue.Min as PQ
import Data.Set (Set)
import Data.Map (Map)
import Data.PQueue.Min (MinQueue)
import Data.Foldable (foldl')

type Point = (Int, Int)

data Direction = North | South | East | West 
    deriving (Eq, Ord, Enum, Bounded)

type State = (Point, Direction)

data Grid = Grid
    { gridPoints :: Set Point
    , gridStart  :: Point
    , gridEnd    :: Point
    } deriving (Show)

makeGrid :: [String] -> Grid
makeGrid input = Grid {..}
  where
    allPoints = 
        [ (row, col, char)
        | (row, line) <- zip [0..] input
        , (col, char) <- zip [0..] line
        ]
    gridPoints = S.fromList [(r, c) | (r, c, ch) <- allPoints, ch `elem` ".SE"]
    gridStart  = head [(r, c) | (r, c, ch) <- allPoints, ch == 'S']
    gridEnd    = head [(r, c) | (r, c, ch) <- allPoints, ch == 'E']

move :: Direction -> Point -> Point
move dir (row, col) = case dir of
    North -> (row - 1, col)
    South -> (row + 1, col)
    East  -> (row, col + 1)
    West  -> (row, col - 1)

turnCost :: Direction -> Direction -> Int
turnCost from to
    | from == to = 0
    | otherwise  = 1000

neighbors :: Grid -> State -> [(State, Int)]
neighbors Grid{..} (pos, dir) =
    [ ((newPos, newDir), moveCost)
    | newDir <- [minBound..maxBound]  -- All possible directions
    , let newPos = move newDir pos
    , newPos `S.member` gridPoints    -- Check if new position is valid
    , let moveCost = 1 + turnCost dir newDir
    ]

findMinScore :: Grid -> Int
findMinScore grid@Grid{..} = search initSeen initQueue
  where
    -- Initialize priority queue with starting position facing East
    initQueue = PQ.singleton (0, (gridStart, East))
    initSeen = M.empty

    search :: Map State Int -> MinQueue (Int, State) -> Int
    search seen queue = case PQ.minView queue of
        Nothing -> maxBound  -- No path found
        Just ((cost, state@(pos, _)), queue')
            | pos == gridEnd -> cost  -- Reached end
            | Just prevCost <- M.lookup state seen
            , prevCost <= cost -> search seen queue'  -- Already found better path
            | otherwise -> search seen' queue''
          where
            seen' = M.insert state cost seen
            queue'' = foldl' (insertNeighbor cost) queue' (neighbors grid state)

insertNeighbor :: Int -> MinQueue (Int, State) -> (State, Int) -> MinQueue (Int, State)
insertNeighbor baseCost queue (state, extraCost) = 
    PQ.insert (baseCost + extraCost, state) queue

main :: IO ()
main = do
    input <- lines <$> readFile "input/day16.txt"
    let grid = makeGrid input

    print $ findMinScore grid
