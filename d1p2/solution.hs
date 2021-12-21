{-# LANGUAGE BangPatterns #-}

delay xs = 0 : xs

main = do
    input <- readFile "./input"
    let sweep = map read . lines $ input
    let blocks_of_3 = drop 2 $ zipWith3 (\a b c -> a + b + c) sweep (delay sweep) (delay (delay sweep))
    let solution = count_times_greater_than_previous blocks_of_3
    print solution

count_times_greater_than_previous :: [Int] -> Int
count_times_greater_than_previous xs = let (n, _) = foldr go (0, -2222) xs in n
    where
        go x (!n, y)
            = if x < y 
                then (n + 1, x)
                else (n, x)
            