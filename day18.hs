import qualified Data.Set as S
import Data.Set (Set)
import Data.Maybe (isJust)

type Point = (Int, Int)
type Grid = Set Point

-- Grid creation
makeGrid :: Point -> Set Point -> Grid
makeGrid (xBound, yBound) blocked = S.fromList
    [ (x, y) 
    | x <- [0..xBound]
    , y <- [0..yBound]
    , (x,y) `S.notMember` blocked 
    ]

-- Directions
directions :: [Point]
directions = [(1,0),(0,1),(-1,0),(0,-1)]

-- Path finding
findDistance :: Grid -> Point -> Point -> Maybe Int
findDistance grid start end = bfs S.empty [(start, 0)]
  where
    bfs _ [] = Nothing
    bfs visited ((curr@(x,y), dist):queue)
      | curr == end = Just dist
      | curr `S.notMember` grid = bfs visited queue
      | curr `S.member` visited = bfs visited queue
      | otherwise = bfs (S.insert curr visited) (queue ++ nextPoints)
      where nextPoints = [ (p, dist+1) 
                         | (dx, dy) <- directions
                         , let p = (x+dx, y+dy)
                         , p `S.notMember` visited
                         , p `S.member` grid
                         ]

-- Find first blocking point
findMaxBlocked :: Point -> [Point] -> Point
findMaxBlocked bounds allPoints = binarySearch 1 (length allPoints)
  where
    binarySearch low high
      | low == high = allPoints !! low
      | otherwise = 
          let mid = (low + high + 1) `div` 2
          in if isValidSolution mid
             then binarySearch mid high
             else binarySearch low (mid - 1)

    isValidSolution n = 
      let blocked = S.fromList $ take n allPoints
          grid = makeGrid bounds blocked
      in isJust $ findDistance grid (0,0) bounds

-- Input parsing
parseInput :: String -> [Point]
parseInput = map parseLine . lines
  where
    parseLine s = let (x, _:y) = break (==',') s
                  in (read x, read y)

-- Main program
main :: IO ()
main = do
    allPoints <- parseInput <$> readFile "input/day18.txt"
    let bounds = (70, 70)
        blocked = S.fromList $ take 1024 allPoints
        grid = makeGrid bounds blocked

    print $ findDistance grid (0,0) bounds
    print $ findMaxBlocked bounds allPoints
