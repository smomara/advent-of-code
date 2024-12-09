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

findGuard :: Grid -> Point
findGuard = head . M.keys . M.filter (== '^')

data Direction = North | East | South | West deriving (Eq, Show, Ord)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

movePoint :: Point -> Direction -> Point
movePoint (r, c) North = (r-1, c)
movePoint (r, c) South = (r+1, c)
movePoint (r, c) East = (r, c+1)
movePoint (r, c) West = (r, c-1)

isInBounds :: Grid -> Point -> Bool
isInBounds grid p = M.member p grid

trackPath :: Grid -> Point -> Direction -> Set Point -> Set Point
trackPath grid pos dir visited = 
    let nextPos = movePoint pos dir
    in if not (isInBounds grid nextPos) then visited
       else if M.findWithDefault '.' nextPos grid == '#' 
            then trackPath grid pos (turnRight dir) (S.insert pos visited)
            else trackPath grid nextPos dir (S.insert nextPos visited)

type VisitedState = Set (Point, Direction)

detectLoop :: Grid -> Point -> Direction -> Point -> VisitedState -> Bool 
detectLoop grid pos dir blockPos visited = 
    let nextPos = movePoint pos dir
    in if nextPos == blockPos 
       then detectLoop grid pos (turnRight dir) blockPos visited
       else if not (isInBounds grid nextPos)
            then False 
            else if M.findWithDefault '.' nextPos grid == '#'
                 then detectLoop grid pos (turnRight dir) blockPos visited
                 else let state = (nextPos, dir)
                      in if state `S.member` visited
                         then True
                         else detectLoop grid nextPos dir blockPos (S.insert state visited)

main :: IO ()
main = do
    input <- makeGrid . lines <$> readFile "input/day06.txt"
    let guardPos = findGuard input
        visited = trackPath input guardPos North (S.singleton guardPos)
        loopPoints = filter (\p -> p /= guardPos && 
                                 detectLoop input guardPos North p (S.singleton (guardPos, North)))
                           (S.toList visited)
    print $ S.size visited
    print $ length loopPoints
