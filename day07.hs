import System.IO (readFile)

valid :: ([Int] -> [Int]) -> [Int] -> Bool
valid f (target:xs) = any (==target) (f xs)

part1 :: [Int] -> [Int]
part1 [x] = [x]
part1 (x:y:rest) = part1 ((x+y):rest) ++ part1 ((x*y):rest)

(.||.) :: Int -> Int -> Int
(.||.) a b = read (show a ++ show b)

part2 :: [Int] -> [Int]
part2 [x] = [x]
part2 (x:y:rest) = part2 ((x+y):rest) ++ part2 ((x*y):rest) ++ part2 ((x .||. y):rest)

main :: IO ()
main = do
    input <- map (map read . words . filter (/=':')) . lines <$> readFile "input/day07.txt"
    print . sum . map head . filter (valid part1) $ input
    print . sum . map head . filter (valid part2) $ input
