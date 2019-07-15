module ProjectEuler where

import Library

-- What is the sum of all the multiples of 3 or 5 less than 1000?
problem1 = sum [ n | n <- [1..1000-1], divBy 3 n || divBy 5 n ]

-- What is the sum of all the even fibonacci numbers under 4 million?
problem2 = sum $ filter (divBy 2) $ takeWhile (<4000000) fibonacci

-- What is the largest prime factor of 600851475143?
problem3 = maximum (factorize 600851475143)

-- What is the largest palindrome made of the product of two 3-digit numbers?
problem4 = maximum [n | n <- nums, isPalindrome (digits n)]
  where nums = [x*y | x <- [900..999], y <- [x..999]]

