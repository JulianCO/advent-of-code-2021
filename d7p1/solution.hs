-- stack --resolver lts-18.18 script --package split --package array

import Data.List (sort)

useExample = False

main = do
    input <- readFile (if useExample then "./ex_input" else "./input")
    let numbers = (read ("[" ++ input ++ "]")) :: [Int]
    let medianNumbers = median numbers
    let targetPosition = head medianNumbers
    let fuelSpent = sum . map (\x -> abs (x-targetPosition)) $ numbers
    print $ maximum numbers 
    print $ minimum numbers 
    print fuelSpent


median :: [Int] -> [Int]
median xs 
    | length xs `mod` 2 == 1 = take 1 . drop (length xs `div` 2) $ sortedXs
    | otherwise = take 2 . drop ((length xs `div` 2)-1) $ sortedXs
        where
            sortedXs = sort xs

