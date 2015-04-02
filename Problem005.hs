main = print $ smallestDivisibleBy [1..20]

smallestDivisibleBy :: [Integer] -> Integer
smallestDivisibleBy xs = foldl1 lcm xs
