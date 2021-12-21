main = do
    input <- readFile "./input"
    let sweep = map read . lines $ input
    let delay_sweep = (444 :: Int) : sweep
    let greater_than_previous = zipWith (>) sweep delay_sweep
    let solution = length (filter id greater_than_previous)
    print solution

