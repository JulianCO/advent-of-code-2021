-- stack --resolver lts-18.18 script --package text --package attoparsec 
{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Attoparsec.Text as P
import Data.Foldable (foldl')
import Data.Either (fromRight)

useExample = False

main = do
    input <- TIO.readFile (if useExample then "./ex_input" else "./input")
    let allDisplays = fromRight (error "parse failed") $ P.parseOnly inputParser input
    --let solution = filter (\xs -> let l = length xs in l == 0). concatMap snd
    let displayedDigits = concatMap snd allDisplays
    let solution = length $ filter (\xs -> (length xs) `elem` [2,3,4,7]) displayedDigits
    print solution

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