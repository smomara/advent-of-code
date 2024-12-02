import System.IO (readFile)
import Data.List (sort, transpose)

main :: IO ()
main = do
    [xs, ys] <- transpose . map (map read . words) . lines <$> readFile "input/day01.txt"
    print $ sum $ zipWith (\x y -> abs (x - y)) (sort xs) (sort ys)
    print $ sum [x * length (filter (==x) ys) | x <- xs]
