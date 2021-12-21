-- stack --resolver lts-18.18 script --package array
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}


import Data.Char (isDigit, digitToInt)
import Data.Array

useExample = False

main = do
    input <- readFile (if useExample then "./ex_input" else "./input")
    let height = length . takeWhile isDigit $ input
    let width = length . lines $ input
    let numbersList = (map digitToInt) . concat . lines $ input
    let numbersArray = listArray ((0,0),(width-1,height-1)) numbersList
    let lowPointsOnly = allLowPoints numbersArray width height
    let solution = sum . map (+1) $ lowPointsOnly
    print $ solution

isLowPoint :: Array (Int,Int) Int -> (Int,Int) -> Int -> Int -> Bool
isLowPoint xs i w h =
    let neighbors = map (xs !) (getNeighborCoordinates w h i)
    in
        all (> (xs!i)) neighbors

allLowPoints arr w h = do
    x <- [0..w-1]
    y <- [0..h-1]
    if isLowPoint arr (x,y) w h then
        [arr!(x,y)]
    else 
        []



getNeighborCoordinates w h (x,y) =
    concat
        [ if x > 0 then [(x-1, y)] else []
        , if x < w-1 then [(x+1,y)] else []
        , if y > 0 then [(x, y-1)] else []
        , if y < h-1 then [(x,y+1)] else []
        ]