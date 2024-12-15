import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Data.List (find, mapAccumL)

-- | Part 1
parse :: [Int] -> [Maybe Int]
parse = go 0
  where
    go _ [] = []
    go i [x] = replicate x (Just i)
    go i (x:y:rest) = replicate x (Just i) ++ replicate y Nothing ++ go (i+1) rest

merge :: [Maybe Int] -> [Maybe Int] -> [Int]
merge (Just x : xs) ys = x : merge xs ys
merge (Nothing : xs) (Just y : ys) = y : merge xs ys
merge xs (Nothing : ys) = merge xs ys
merge _ _ = []

compress :: [Int] -> [Int]
compress input = take validLen $ merged
  where
    p = parse input
    merged = merge p (reverse p)
    validLen = length (catMaybes p)

checksum :: [Int] -> Int
checksum = sum . zipWith (*) [0..]

-- | Part 2
type File = (Int, Int, Int) -- position, size, id
type Empty = (Int, Int) -- position, size

parseToBlocks :: [Int] -> ([File], [Empty])
parseToBlocks nums = go 0 0 nums
  where
    go _ _ [] = ([], [])
    go pos fid [x] = ([(pos, x, fid)], [])
    go pos fid (x:y:rest) =
      let (files, empties) = go (pos + x + y) (fid + 1) rest
      in ((pos, x, fid):files, (pos + x, y):empties)

moveBlock :: [Empty] -> File -> ([Empty], [Int])
moveBlock empties (pos, size, fid) = (newEmpties, [contribution])
  where
    foundGap = find (\(gapPos, gapSize) -> gapPos < pos && gapSize >= size) empties

    contribution = case foundGap of
      Nothing -> fid * sum [pos..pos+size-1]
      Just (gapPos, _) -> fid * sum [gapPos..gapPos+size-1]

    newEmpties = case foundGap of
      Nothing -> empties
      Just (gapPos, gapSize) ->
          let (before, after) = span (/= (gapPos, gapSize)) empties
              remainingGaps = if gapSize > size
                            then (gapPos + size, gapSize - size) : tail after
                            else tail after
          in before ++ remainingGaps

compress' :: [File] -> [Empty] -> Int
compress' fs es = sum $ concat $ snd $ mapAccumL moveBlock es (reverse fs)

main :: IO ()
main = do
    input <- map digitToInt . head . lines <$> readFile "input/day09.txt"
    print . checksum . compress $ input
    print . uncurry compress' $ parseToBlocks input
