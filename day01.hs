import System.IO (readFile)
import Data.List (sort)
import qualified Data.Map as Map

split (x:y:rest) = let (xs, ys) = split rest in (x:xs,y:ys)
split _ = ([], [])

main :: IO ()
main = do
    nums <- map read . words <$> readFile "input/day01.txt"
    let (xs, ys) = split nums
        freqs = foldr (\x -> Map.insertWith (+) x 1) Map.empty ys
    print $ sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)
    print $ sum [x * Map.findWithDefault 0 x freqs | x <- xs]

