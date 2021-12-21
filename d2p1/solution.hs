import Data.List (stripPrefix)
import Data.Maybe (fromJust)

main = do
    input <- readFile "./input"
    let instructions = lines input
    let (position_h, position_v) = fromJust $ applyInstructions instructions
    let solution = position_h * position_v
    print solution

applyInstructions :: [String] -> Maybe (Int, Int)
applyInstructions [] = Just (0,0)
applyInstructions (i:is) = do
    (remaining_h, remaining_v) <- applyInstructions is
    case i of
        ('f':cs) -> do
            stepsForward <- read <$> stripPrefix "orward " cs
            return (remaining_h + stepsForward, remaining_v)
        ('d':cs) -> do
            stepsDown <- read <$> stripPrefix "own " cs
            return (remaining_h, remaining_v + stepsDown)
        ('u':cs) -> do
            stepsUp <- read <$> stripPrefix "p " cs
            return (remaining_h, remaining_v - stepsUp)
        _ -> Nothing
