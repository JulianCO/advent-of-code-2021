-- stack --resolver lts-18.18 script
{-# LANGUAGE OverloadedStrings #-}

data Delimiter = Curly | Square | Paren | Angle deriving (Show)

data ParseResult = Partial [Char] | ParseError Delimiter deriving (Show)

useExample = False

main = do
    input <- readFile (if useExample then "./ex_input" else "./input")
    let allChunks = lines input
    let parsedChunks = map (parseChunk []) allChunks
    let solution = sum . map errorScore $ parsedChunks
    print solution

errorScore (Partial _) = 0
errorScore (ParseError u) =
    case u of
        Curly -> 1197
        Square -> 57
        Paren -> 3
        Angle -> 25137

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



