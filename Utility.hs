module Utility where

factorize :: Integer -> [Integer]
factorize n = tryDiv n (2:[3,5..])

tryDiv :: Integer -> [Integer] -> [Integer]
tryDiv n (x:xs)
    | x^2 > n    = [n]
    | r == 0     = x : tryDiv q (x:xs)
    | otherwise  = tryDiv n xs
        where
            (q,r) = divMod n x