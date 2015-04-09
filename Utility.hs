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
 
primes :: [Integer]
primes = 2 : sieve primes [3..] 
    where
        sieve (p:ps) xs =
            let (h,t) = span (< p*p) xs 
            in h ++ sieve ps [x | x<-t, rem x p /= 0]
