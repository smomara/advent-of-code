import Data.Bits (xor)
import Data.Char (digitToInt, intToDigit)
import Data.List (intersperse)

type Registers = (Int, Int, Int)  -- (A, B, C)
type Program = [Int]
type IP = Int

evalCombo :: Registers -> Int -> Int
evalCombo (a, b, c) arg = case arg of
    0 -> 0
    1 -> 1
    2 -> 2
    3 -> 3
    4 -> a
    5 -> b
    6 -> c
    _ -> error "bad combo"

execInstr :: Registers -> Int -> Int -> (Registers, Maybe Int)
execInstr (a, b, c) op arg = case op of
    0 -> ((a `div` (2 ^ val), b, c), Nothing)        -- adv
    1 -> ((a, b `xor` arg, c), Nothing)              -- bxl
    2 -> ((a, val `mod` 8, c), Nothing)              -- bst
    3 -> ((a, b, c), Nothing)                        -- jnz (handled in run)
    4 -> ((a, b `xor` c, c), Nothing)                -- bxc
    5 -> ((a, b, c), Just (val `mod` 8))             -- out
    6 -> ((a, a `div` (2 ^ val), c), Nothing)        -- bdv
    7 -> ((a, b, a `div` (2 ^ val)), Nothing)        -- cdv
    _ -> error "bad opcode"
  where
    val = evalCombo (a, b, c) arg

nextIP :: Int -> Int -> Registers -> IP -> IP
nextIP op arg (a,_,_) ip = 
    if op == 3 && a /= 0 
    then arg 
    else ip + 2

run :: Registers -> Program -> [Int]
run regs prog = go regs 0 []
  where
    go regs@(a,b,c) ip outputs
        | ip >= length prog = reverse outputs
        | otherwise = 
            let op = prog !! ip
                arg = prog !! (ip + 1)
                (regs', out) = execInstr regs op arg
                newOutputs = maybe outputs (:outputs) out
                ip' = nextIP op arg regs ip
            in go regs' ip' newOutputs

parseRegister :: String -> Int
parseRegister = read . last . words

parseProgram :: String -> Program
parseProgram = map digitToInt . filter (/=',') . drop 9  -- drop "Program: "

parseInput :: String -> (Registers, Program)
parseInput input =
    let ls = lines input
        a = parseRegister $ ls !! 0
        b = parseRegister $ ls !! 1
        c = parseRegister $ ls !! 2
        prog = parseProgram $ ls !! 4
    in ((a, b, c), prog)

main :: IO ()
main = do
    (regs,prog) <- parseInput <$> readFile "input/day17.txt"
    putStrLn . intersperse ',' . map intToDigit $ run regs prog
