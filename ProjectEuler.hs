module ProjectEuler where

problem1 = sum [ n | n <- [1..1000-1], divBy 3 n || divBy 5 n ]

problem2 = sum $ filter (divBy 2) $ takeWhile (<4000000) fibonacci

problem3 = maximum (factorize 600851475143)

divBy :: Integral a => a -> a -> Bool
divBy d n = n `mod` d == 0

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

factorize :: Integer -> [Integer]
factorize 1 = []
factorize n = tryDiv n (2:[3,5..])
  where
    tryDiv :: Integer -> [Integer] -> [Integer]
    tryDiv n (x:xs)
      | x^2 > n  = [n]  -- Stop when we've gotten to the square root of n.
      | r == 0  = x : tryDiv q (x:xs)  -- Success, add the divisor and continue.
      | otherwise  = tryDiv n xs  -- Failure, keep trying.
        where (q,r) = divMod n x
