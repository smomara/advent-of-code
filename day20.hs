import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

type Point = (Int, Int)
type Grid = Map Point Int  -- point -> distance from end

makeGrid :: [String] -> Grid
makeGrid input = bfs walkable end
  where
    points =
        [ (row, col, char)
        | (row, line) <- zip [0..] input
        , (col, char) <- zip [0..] line
        ]
    walkable = S.fromList [(r, c) | (r, c, ch) <- points, ch `elem` ".SE"]
    end = head [(r, c) | (r, c, ch) <- points, ch == 'E']

bfs :: Set Point -> Point -> Map Point Int
bfs walkable end = go (M.singleton end 0) [(end, 0)]
  where
    go dists [] = dists
    go dists ((pos, dist):queue) =
        let neighbors = [ neighbor
                        | (dx, dy) <- [(1,0),(-1,0),(0,1),(0,-1)]
                        , let (x, y) = pos
                        , let neighbor = (x+dx, y+dy)
                        , neighbor `S.member` walkable
                        , neighbor `M.notMember` dists
                        ]
            dists' = foldr (`M.insert` (dist+1)) dists neighbors
            queue' = queue ++ [(p, dist+1) | p <- neighbors]
        in go dists' queue'

findCheats :: Map Point Int -> Point -> Int -> [Int]
findCheats distMap pos maxDist = 
    [ timeSaved
    | (dx, dy) <- [(x, y) | x <- [-maxDist..maxDist], y <- [-maxDist..maxDist]]
    , let manhattan = abs dx + abs dy
    , manhattan <= maxDist && manhattan > 0
    , let targetPos = addPoint pos (dx, dy)
    , targetPos `M.member` distMap
    , let targetDist = distMap M.! targetPos
    , let currentDist = distMap M.! pos
    , let timeSaved = currentDist - (targetDist + manhattan)
    , timeSaved > 0
    ]
  where
    addPoint (x1, y1) (x2, y2) = (x1+x2, y1+y2)

main :: IO ()
main = do
    grid <- makeGrid . lines <$> readFile "input/day20.txt"

    let all2Cheats = [ saved | point <- M.keys grid
                     , saved <- findCheats grid point 2 ]
    print . length $ filter (>=100) all2Cheats

    let all20Cheats = [ saved | point <- M.keys grid
                      , saved <- findCheats grid point 20 ]
    print . length $ filter (>=100) all20Cheats
