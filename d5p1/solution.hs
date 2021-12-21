-- stack --resolver lts-18.18 script --package text --package attoparsec --package containers

{-# LANGUAGE OverloadedStrings #-}
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Attoparsec.Text as P
import Data.Foldable (foldl')
import Data.Either (fromRight)

data Point = Point Int Int deriving (Show)
data Line = Line Point Point deriving (Show)

useExample = False

main = do
    input <- TIO.readFile (if useExample then "./ex_input" else "./input")
    let allLines = fromRight (error "parse failed") $ P.parseOnly inputParser input
    let allNumbers = concatMap lineToList allLines
    let solution = countRepeatedNumbers allNumbers
    print solution

oneDLine :: Int -> Int -> [Int]
oneDLine x1 x2
    | x1 > x2 = [x2..x1]
    | otherwise = [x1..x2]

lineToList :: Line -> [Int]
lineToList (Line (Point x1 y1) (Point x2 y2))
    = if x1 == x2
        then
            [1000*i + x1 | i <- oneDLine y1 y2]
        else
            if y1 == y2
                then 
                    [1000*y1 + i | i <- oneDLine x1 x2]
            else
                []

pointParser :: P.Parser Point
pointParser = Point <$> P.decimal <*> ("," *> P.decimal)

lineParser :: P.Parser Line
lineParser = (Line <$> pointParser <*> (" -> " *> pointParser))

inputParser :: P.Parser [Line]
inputParser = P.sepBy lineParser P.endOfLine

countRepeatedNumbers :: [Int] -> Int
countRepeatedNumbers = Set.size . snd .countRepeatedNumbers'

countRepeatedNumbers' :: [Int] -> (Set Int, Set Int)
countRepeatedNumbers' xs = foldl' go (Set.empty, Set.empty) xs
    where
        go (s1, s2) x
            | Set.member x s1 = (s1, Set.insert x s2)
            | otherwise = (Set.insert x s1, s2)

