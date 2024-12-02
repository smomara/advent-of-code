import System.IO (readFile)
import Data.Char (digitToInt)

checkSequence :: [Int] -> Bool
checkSequence (x:y:xs) =
    let diffs = zipWith (-) (y:xs) (x:y:xs)
    in (all (>0) diffs || all (<0) diffs) && all (\d -> 1 <= abs d && abs d <= 3) diffs

canBeValid :: [Int] -> Bool
canBeValid xs = any checkSequence [take i xs ++ drop (i+1) xs | i <- [0..length xs - 1]]

main :: IO ()
main = do
    file <- readFile "input/day02.txt"
    let nums = map (map (read :: String -> Int) . words) . lines $ file
    print $ length . filter id . map checkSequence $ nums
    print $ length . filter id . map canBeValid $ nums

