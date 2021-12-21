import Data.Char (digitToInt)
import Data.Foldable (foldl')

useExample = False

main = do
    let filename = if useExample then "./ex_input" else "./input"
    input <- readFile filename
    let binaryStrings = map (map digitToInt) . lines  $ input
    let oxygen = digitListToNumber . og_rating $ binaryStrings
    let co2 = digitListToNumber . cs_rating $ binaryStrings
    let solution = oxygen*co2
    print solution

og_rating :: [[Int]] -> [Int]
og_rating [] = []
og_rating (x:[]) = x
og_rating xs 
    =  let firstDigits = map head xs in
        if 2*(sum firstDigits) >= length xs
        then
            1:(og_rating . map tail . filter ((== 1) . head) $ xs)
        else
            0:(og_rating . map tail . filter ((== 0) . head) $ xs)

cs_rating :: [[Int]] -> [Int]
cs_rating [] = []
cs_rating (x:[]) = x
cs_rating xs 
    =  let firstDigits = map head xs in
        if 2*(sum firstDigits) >= length xs
        then
            0:(cs_rating . map tail . filter ((== 0) . head) $ xs)
        else
            1:(cs_rating . map tail . filter ((== 1) . head) $ xs)

digitListToNumber :: [Int] -> Int
digitListToNumber = foldl' (\n d -> n*2 + d) 0