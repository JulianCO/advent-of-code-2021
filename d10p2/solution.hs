-- stack --resolver lts-18.18 script
{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)

data Delimiter = Curly | Square | Paren | Angle deriving (Show)

data ParseResult = Partial [Char] | ParseError Delimiter deriving (Show)

useExample = False

main = do
    input <- readFile (if useExample then "./ex_input" else "./input")
    let allChunks = lines input
    let parsedChunks = map (parseChunk []) allChunks
    let completionScores = filter (>0) . map completionScore $ parsedChunks
    let solution = head . drop (length completionScores `div` 2) . sort $ completionScores
    print solution

completionScore (ParseError _) = 0
completionScore (Partial cs) = foldl go 0 cs
    where
        go x y = 5*x + delimiterScore (charToDelimiter y)

delimiterScore d =
    case d of
        Curly -> 3
        Square -> 2
        Paren -> 1
        Angle -> 4


charToDelimiter c =
    case c of
        ']' -> Square
        '}' -> Curly
        ')' -> Paren
        '>' -> Angle

parseChunk :: String -> String -> ParseResult
parseChunk stack [] = Partial stack
parseChunk stack ('(':cs) = parseChunk (')':stack) cs
parseChunk stack ('[':cs) = parseChunk (']':stack) cs
parseChunk stack ('{':cs) = parseChunk ('}':stack) cs
parseChunk stack ('<':cs) = parseChunk ('>':stack) cs
parseChunk (s:stack) (c:cs) 
    | c == s = parseChunk stack cs
    | otherwise = ParseError (charToDelimiter c)



