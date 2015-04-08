main = print problem6

problem6 = squaredSum [1..100] - sumOfSquares [1..100]

sumOfSquares :: [Integer] -> Integer
sumOfSquares xs = sum (map (^2) xs)

squaredSum :: [Integer] -> Integer
squaredSum xs = sum xs ^ 2
