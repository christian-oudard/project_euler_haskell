module Utility where

factorize :: Integer -> [Integer]
factorize 1 = []
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
      let (h,t) = span (< p^2) xs
      in h ++ sieve ps [x | x<-t, rem x p /= 0]

digits :: Integer -> [Integer]
digits n = case n of
  0 -> []
  _ -> digits q ++ [r]
    where
      (q,r) = divMod n 10

unDigits :: [Integer] -> Integer
unDigits ds = foldl (\a b -> 10*a + b) 0 ds

merge as [] = as
merge [] bs = bs
merge (a:as) (b:bs)
  | a <= b     = a : merge as (b:bs)
  | otherwise  = b : merge (a:as) bs

factorial n = product [1..n]
