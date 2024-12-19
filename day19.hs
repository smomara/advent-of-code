import Control.Arrow ((&&&))
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (fromMaybe)

data Trie = Node Bool (Map Char Trie) deriving Show

empty :: Trie
empty = Node False M.empty

insert :: String -> Trie -> Trie
insert "" (Node _ children) = Node True children
insert (c:cs) (Node end children) =
    Node end (M.alter (Just . insert cs . fromMaybe empty) c children)

makeTrie :: [String] -> Trie
makeTrie = foldr insert empty

canMake :: Trie -> String -> Int
canMake root str = ways 0
  where
    ways = (memo M.!)
    memo = M.fromList [(i, count root i) | i <- [0..n]]
    n = length str

    count (Node end children) i
        | i == n = fromEnum end
        | otherwise = matchHere + continuePattern
        where
            matchHere = fromEnum end * ways i
            continuePattern = maybe 0 (`count` (i+1))
                            $ M.lookup (str !! i) children

main :: IO ()
main = do
    (patterns, designs) <- parseInput <$> readFile "input/day19.txt"

    let trie = makeTrie patterns
        combinations = map (canMake trie) designs

    print . length . filter (>0) $ combinations
    print . sum                  $ combinations

parseInput :: String -> ([String], [String])
parseInput = ((map (filter (/=',')) . words . head) &&&
             (filter (not . null) . tail))
             . lines
