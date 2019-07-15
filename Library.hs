module Library where

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

primes :: [Integer]
primes = 2 : sieve primes [3..]
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p ^ 2) xs
      in h ++ sieve ps [x | x<-t, rem x p /= 0]

digits = digitsBase 10
digitsBase :: Integer -> Integer -> [Integer]
digitsBase b n =
  let (q,r) = divMod n b
  in
    case n of
      0 -> []
      _ -> digitsBase b q ++ [r]

undigits :: [Integer] -> Integer
undigits = unDigitsBase 10
unDigitsBase :: Integer -> [Integer] -> Integer
unDigitsBase b = foldl (\x y -> b*x + y) 0

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists as [] = as
mergeLists [] bs = bs
mergeLists (a:as) (b:bs)
  | a <= b     = a : mergeLists as (b:bs)
  | otherwise  = b : mergeLists (a:as) bs

factorial :: (Enum a, Num a) => a -> a
factorial n = product [1..n]
