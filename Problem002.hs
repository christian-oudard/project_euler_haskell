main = print $ problem2 4000000

problem2 limit = sum [n | n <- nums, n `mod` 2 == 0]
    where nums = takeWhile (<limit) fibs

fibs :: [Integer]
fibs = 0:1: zipWith (+) fibs (tail fibs)
