import System.IO (readFile)
import Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

type Node = Int
type Graph = Map Node (Set Node)

splitOn :: (a -> Bool) -> [a] -> ([a], [a])
splitOn f = fmap tail . break f

parseNode :: String -> (Node, Node)
parseNode = (\(a,b) -> (read a, read b)) . splitOn (=='|')

parsePages :: String -> [Node]
parsePages = map read . words . map (\c -> if c == ',' then ' ' else c)

buildGraph :: [(Node, Node)] -> Graph
buildGraph = foldr (\(a,b) -> M.insertWith S.union a (S.singleton b)) M.empty

isValidOrder :: Graph -> [Node] -> Bool
isValidOrder g pages = all checkOrder (zip [0..] pages)
  where
    pos = M.fromList $ zip pages [0..]
    checkOrder (i, p) = case M.lookup p g of
        Nothing -> True
        Just deps -> all (\d -> maybe True (> i) $ M.lookup d pos) $ S.toList deps

middlePage :: [Node] -> Node
middlePage xs = xs !! (length xs `div` 2)

topologicalSort :: Graph -> [Node] -> [Node]
topologicalSort graph nodes = sortBy compareNodes nodes
  where
    compareNodes a b = case (M.lookup a graph, M.lookup b graph) of
        (Just depsA, Just depsB) -> 
            if S.member b depsA then GT
            else if S.member a depsB then LT
            else EQ
        (Just _, Nothing) -> GT
        (Nothing, Just _) -> LT
        (Nothing, Nothing) -> EQ

main :: IO ()
main = do
    (s1, s2) <- splitOn null . lines <$> readFile "input/day05.txt"
    let graph = buildGraph $ map parseNode s1
        pages = map parsePages s2
        validPages = filter (isValidOrder graph) pages
        invalidPages = filter (not . isValidOrder graph) pages
        sortedInvalidPages = map (topologicalSort graph) invalidPages
    print . sum . map middlePage $ validPages
    print . sum . map middlePage $ sortedInvalidPages
