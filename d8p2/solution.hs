-- stack --resolver lts-18.18 script --package text --package attoparsec 
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Attoparsec.Text as P
import Data.Foldable (foldl')
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.List (partition, sort, elemIndex)

useExample = False

main = do
    input <- TIO.readFile (if useExample then "./ex_input" else "./input")
    let allDisplays = fromRight (error "parse failed") $ P.parseOnly inputParser input
    let withCablesUntangled = map (\(xs, ys) -> (untangleCables xs, map sort ys)) allDisplays
    let decodedNumbers = map (\(cables, display) -> decodeDisplay cables display) withCablesUntangled
    let solution = sum decodedNumbers
    print solution
    --putStrLn . unlines . map show . take 4 $ withCablesUntangled

fitsInto :: [Char] -> [Char] -> Bool
fitsInto xs ys = all (`elem` ys) xs

untangleCables :: [[Char]] -> [[Char]]
untangleCables xs =
    let
        one = head . filter (\xs -> length xs == 2) $ xs
        seven = head . filter (\xs -> length xs == 3) $ xs
        four = head . filter (\xs -> length xs == 4) $ xs
        eight = head . filter (\xs -> length xs == 7) $ xs
        sixSegments = filter (\xs -> length xs == 6) xs
        (nine:[], otherSixSegs) = partition (\d -> fitsInto four d) sixSegments
        (zero:[], six:[]) = partition (\d -> fitsInto one d) otherSixSegs
        fiveSegments = filter (\xs -> length xs == 5) xs
        (three:[], otherFiveSegs) = partition (\d -> fitsInto one d) fiveSegments
        (five:[], two:[]) = partition (\d -> fitsInto d six) otherFiveSegs
    in
        map sort [zero, one, two, three, four, five, six, seven, eight, nine]

decodeDisplay :: [[Char]] -> [[Char]] -> Int
decodeDisplay untangledSegments d =
    let theDigits = map (fromJust . (`elemIndex` untangledSegments)) d 
    in 
        digitsToNumber theDigits

digitsToNumber :: [Int] -> Int
digitsToNumber xs = foldl' (\x y -> 10*x + y) 0 xs

digitsSegment :: P.Parser [Char]
digitsSegment = P.many1 P.letter

manyDigitsSegment :: P.Parser [[Char]]
manyDigitsSegment = P.sepBy digitsSegment " "

aDisplay :: P.Parser ([[Char]], [[Char]])
aDisplay = do
    allConfigurations <- manyDigitsSegment
    " | "
    displayedDigits <- manyDigitsSegment
    pure (allConfigurations, displayedDigits)

inputParser :: P.Parser [([[Char]], [[Char]])]
inputParser = P.sepBy aDisplay P.endOfLine