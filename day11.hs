import Data.Char (digitToInt)
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)

freqs :: [Int] -> IntMap Int
freqs = IM.fromListWith (+) . map (,1)

stepMap :: IntMap Int -> IntMap Int
stepMap mp = IM.unionsWith (+)
  [ (*n) <$> freqs (step x)
  | (x, n) <- IM.toList mp
  ]

step :: Int -> [Int]
step 0 = [1]
step n
  | even digits = [a, b]
  | otherwise   = [n * 2024]
  where
    digits = length $ takeWhile (<= n) $ iterate (*10) 1
    (a, b) = n `divMod` (10 ^ (digits `div` 2))

main :: IO ()
main = do
    input <- map read . words <$> readFile "input/day11.txt"
    print . sum . (!!25) . iterate stepMap . freqs $ input
    print . sum . (!!75) . iterate stepMap . freqs $ input
