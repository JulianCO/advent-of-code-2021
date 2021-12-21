import Data.List (elemIndex)
import Data.Bits ((.|.), (.&.))
import Data.List.Split (splitOn)

data BingoBoard = BingoBoard [Int] Int deriving (Show)

useExample = True

magicNumbers = do
    row <- [True, False]
    if row then do
        rowNumber <- [0,1,2,3,4]
        return (sum [2^(5*rowNumber + i) | i <- [0,1,2,3,4]])
    else do
        columnNumber <- [0,1,2,3,4]
        return (sum [2^(5*i + columnNumber) | i <- [0,1,2,3,4]])

callNumber :: Int -> BingoBoard -> BingoBoard
callNumber n (BingoBoard ns marked) =
    case elemIndex n ns of
        Nothing ->
            BingoBoard ns marked
        Just i -> 
            BingoBoard ns (marked .|. (2^i))

hasWon :: BingoBoard -> Bool
hasWon (BingoBoard _ marked) = any (\mn -> (mn .&. marked) == mn) magicNumbers

-- Takes list of called numbers and a list of boards and returns
-- The winning board and the last number called
lastWinningBoard :: [Int] -> [BingoBoard] -> (BingoBoard, Int)
lastWinningBoard called boards = 
    let
        boards' = map (callNumber (head called)) boards
        loserBoards = filter (not . hasWon) boards'
    in
        case loserBoards of
            [] -> (head boards', head called)
            _ -> lastWinningBoard (tail called) loserBoards

parseBingoBoard :: [String] -> BingoBoard
parseBingoBoard ss = BingoBoard xs 0
    where
        xs = map read . concatMap words $ ss

bingoScore :: BingoBoard -> Int -> Int
bingoScore (BingoBoard ns marked) lastCalled = (sum unmarked)*lastCalled
    where
        boardWithValues = zip ns [2^i | i <- [0,1..24]]
        unmarked' = filter (\(_,v) -> v .&. marked /= v) boardWithValues
        unmarked = map fst unmarked'

main = do
    input <- lines <$> readFile (if useExample then "./ex_input" else "./input")
    let numbersCalled = read ("[" ++ head input ++ "]") :: [Int]
    let boardStrings = splitOn [""] (drop 2 input)
    let allBoards = map parseBingoBoard boardStrings
    let (loserBoard, lastCalled) = lastWinningBoard numbersCalled allBoards
    --print numbersCalled
    --print allBoards
    --print winnerBoard
    print (bingoScore loserBoard lastCalled)


