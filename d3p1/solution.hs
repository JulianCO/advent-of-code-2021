import Data.Char (digitToInt)
import Data.Foldable (foldl')

useExample = False

main = do
    let filename = if useExample then "./ex_input" else "./input"
    input <- readFile filename
    let binaryStrings = map (map digitToInt) . lines  $ input
    let sumColumns = foldl' (zipWith (+)) [0, 0..] binaryStrings
    let gammaList = map (\x -> if 2*x > length binaryStrings then 1 else 0) sumColumns
    let gamma = foldl (\n d -> n*2 + d) 0 gammaList
    let epsilon = 2^(length (head binaryStrings)) - 1 - gamma
    print gamma
    print epsilon
    print (gamma*epsilon)




