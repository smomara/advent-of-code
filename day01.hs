import System.IO (readFile)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Map (Map)

split (x:y:rest) = let (xs, ys) = split rest in (x:xs,y:ys)
split _ = ([], [])

part1 :: [Int] -> [Int] -> Int
part1 xs ys = sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)

part2 :: [Int] -> Map Int Int -> Int
part2 xs freqs = sum [x * Map.findWithDefault 0 x freqs | x <- xs]

main :: IO ()
main = do
    nums <- map read . words <$> readFile "input/day01.txt"
    let (xs, ys) = split nums
        freqs = foldr (\x -> Map.insertWith (+) x 1) Map.empty ys
    print $ part1 xs ys
    print $ part2 xs freqs

