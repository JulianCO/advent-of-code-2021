-- stack --resolver lts-18.18 script --package array
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}


import Data.Char (isDigit, digitToInt)
import Data.List (find, sort)
import Data.Array

data FlowDirection = Up | Right | Down | Left | Stable | Unstable

useExample = False

main = do
    input <- readFile (if useExample then "./ex_input" else "./input")
    let height = length . takeWhile isDigit $ input
    let width = length . lines $ input
    let numbersList = (map digitToInt) . concat . lines $ input
    let numbersArray = listArray ((0,0),(width-1,height-1)) numbersList
    let heightTree = buildHeightTree numbersArray width height
    let lowPoints = listChildren heightTree (-1,-1)
    let bassinSizes = map (treeSize heightTree) lowPoints
    let solution = product . take 3 . reverse . sort $ bassinSizes
    --let solution = sum . map (+1) $ lowPointsOnly
    print $ solution

isLowPoint :: Array (Int,Int) Int -> (Int,Int) -> Int -> Int -> Bool
isLowPoint xs i w h =
    let neighbors = map (xs !) (getNeighborCoordinates w h i)
    in
        all (> (xs!i)) neighbors


buildHeightTree arr w h = do
    x <- [0..w-1]
    y <- [0..h-1]
    if arr ! (x,y) == 9 then
        []
    else
        case find (\j -> arr!j < arr!(x,y)) (getNeighborCoordinates w h (x,y)) of
            Just j ->
                return ((x,y), j)
            Nothing ->
                return ((x,y), (-1,-1))

listChildren tree i = map fst . filter ((== i) . snd) $ tree

treeSize :: [((Int,Int),(Int,Int))] -> (Int, Int) -> Int
treeSize tree i = 1 + sum (map (treeSize tree) (listChildren tree i))

getNeighborCoordinates w h (x,y) =
    concat
        [ if x > 0 then [(x-1, y)] else []
        , if x < w-1 then [(x+1,y)] else []
        , if y > 0 then [(x, y-1)] else []
        , if y < h-1 then [(x,y+1)] else []
        ]