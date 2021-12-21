import Data.List (stripPrefix, foldl')
import Data.Maybe (fromJust)

use_example = False

data Instruction 
    = Forward Int
    | Down Int
    | Up Int
    deriving (Show)

main = do
    input <- readFile (if use_example then "./ex_input" else "./input")
    --print input
    let instructions = map fromJust . map parseInstruction . lines $ input
    --print instructions
    let (_final_aim, position_h, position_v) = applyInstructions 0 instructions
    let solution = position_h * position_v
    print solution

parseInstruction :: String -> Maybe Instruction
parseInstruction ('f':cs) = Forward . read <$> (stripPrefix "orward " cs)
parseInstruction ('d':cs) = Down . read <$> (stripPrefix "own " cs)
parseInstruction ('u':cs) = Up . read <$> (stripPrefix "p " cs)
parseInstruction _ = Nothing

applyInstructions :: Int -> [Instruction] -> (Int, Int, Int)
applyInstructions aim = foldl' go (aim,0,0)
    where
        go (a, x, y) (Forward n) = (a, x + n, y + a*n)
        go (a, x, y) (Down n) = (a + n, x, y)
        go (a, x, y) (Up n) = (a - n, x, y)
