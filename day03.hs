import System.IO (readFile)
import Text.Regex.TDFA ((=~))
import Data.List.Split (splitOn)

sumMuls :: String -> Int
sumMuls str = sum [read n1 * read n2 | (_:n1:n2:_) <- matches]
    where matches = str =~ "mul\\(([0-9]+),\\s*([0-9]+)\\)" :: [[String]]

processLine :: String -> Int
processLine = sumMuls . concat . map (head . splitOn "don't()") . splitOn ("do()")

main :: IO ()
main = do
    input <- concat . lines <$> readFile "input/day03.txt"
    print . sumMuls $ input
    print . processLine $ input
