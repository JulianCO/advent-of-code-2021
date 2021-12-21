-- stack --resolver lts-18.18 script --package split --package array

import Data.List.Split (splitOn)
import Data.Array (Array, assocs)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST

useExample = False

main = do
    input <- readFile (if useExample then "./ex_input" else "./input")
    let fishList = (map read . splitOn "," $ input)
    let popGraph = [fromIntegral (length (filter (== i) fishList)) | i <- [0..8]]
    print popGraph
    let projectedPopGraph = projectPopulation popGraph
    print $ projectedPopGraph
    let solution = sum . map snd . assocs $ projectedPopGraph
    print solution


projectPopulation :: [Integer] -> Array Int Integer
projectPopulation xs =
    runSTArray $ do
        arr <- newListArray (0,8) xs
        forM_ [0..79] $ (\d -> updatePopulation d arr)
        return arr

updatePopulation :: Int -> STArray s Int Integer -> ST s ()
updatePopulation day arr = do
    let newDay6 = (day + 7) `mod` 9
    nbSpawned <- readArray arr (day `mod` 9) 
    nbStartOfCycle <- readArray arr newDay6
    writeArray arr newDay6 (nbSpawned + nbStartOfCycle)

