-- stack --resolver lts-18.18 script --package split --package array

import Data.List (sort)

useExample = False

main = do
    input <- readFile (if useExample then "./ex_input" else "./input")
    let numbers = (read ("[" ++ input ++ "]")) :: [Int]
    let fuelToZero = sum . map (\x -> x*(x+1) `div` 2) $ numbers
    let mappedFuel = fuelMap fuelToZero (length numbers) (sort numbers) (sum numbers) (length numbers) 0 (maximum numbers)
    let solution = minimum mappedFuel
    print solution


median :: [Int] -> [Int]
median xs 
    | length xs `mod` 2 == 1 = take 1 . drop (length xs `div` 2) $ sortedXs
    | otherwise = take 2 . drop ((length xs `div` 2)-1) $ sortedXs
        where
            sortedXs = sort xs

-- given the fuel consumption for mu, gives the difference in fuel consumption for
-- mu+1. n is the total number of submarines
nextFuelDelta n mu sumXs nbXsGreaterThanMu = n*(mu + 1) - sumXs - nbXsGreaterThanMu

-- must be called with sorted xs, and seeded with the correct first value
fuelMap :: Int -> Int -> [Int] -> Int -> Int -> Int -> Int -> [Int]
fuelMap first n xs sumXs lastXsGreater start end 
    | start > end = 
        []
    | xs == [] = 
        let next = first + (nextFuelDelta n start sumXs 0) in
            first:(fuelMap next n [] sumXs 0 (start+1) end)
    | otherwise =
        if head xs == start then
            fuelMap first n (tail xs) sumXs (lastXsGreater-1) start end 
        else 
            let next = first + (nextFuelDelta n start sumXs lastXsGreater) in
                first:(fuelMap next n xs sumXs lastXsGreater (start+1) end )


