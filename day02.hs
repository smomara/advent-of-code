import System.IO (readFile)

valid :: [Int] -> Bool
valid xs@(_:rest) =
    let diffs = zipWith (-) xs rest
    in (all (>0) diffs || all (<0) diffs) && all (flip elem [1..3] . abs) diffs

valid' :: [Int] -> Bool
valid' xs = any valid [take i xs ++ drop (i+1) xs | i <- [0..length xs - 1]]

main :: IO ()
main = do
    nums <- map (map read . words) . lines <$> readFile "input/day02.txt"
    print $ length . filter id . map valid $ nums
    print $ length . filter id . map valid' $ nums

