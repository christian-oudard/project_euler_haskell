module ProjectEuler where

import Data.Maybe (mapMaybe)

import Library

-- #1 --
-- What is the sum of all the multiples of 3 or 5 less than 1000?
problem1 = sum [ n | n <- [1..1000-1], divBy 3 n || divBy 5 n ]

-- #2 --
-- What is the sum of all the even fibonacci numbers under 4 million?
problem2 = sum $ filter (divBy 2) $ takeWhile (<4000000) fibonacci

-- #3 --
-- What is the largest prime factor of 600851475143?
problem3 = maximum (factorize 600851475143)

-- #4 --
-- What is the largest palindrome made of the product of two 3-digit numbers?
problem4 = maximum [n | n <- nums, isPalindrome (digits n)]
  where nums = [x*y | x <- [900..999], y <- [x..999]]

-- #5 --
-- What is the smallest number divisible by all the numbers from 1 to 20?
problem5 = foldl1 lcm [1..20]

-- #6 --
-- What is the difference between the squared sum of the first 100 numbers, and the sum of
-- squares of the first 100 numbers?
problem6 = squaredSum [1..100] - sumOfSquares [1..100]
  where
    squaredSum xs = sum xs ^ 2
    sumOfSquares xs = sum (map (^2) xs)

-- #7 --
-- What is the 10,001st prime?
problem7 = primes !! 10000

-- #8 --
-- What is the largest product of 13 consecutive digits in this number?
problem8 = maximum $ map product (groupwise 13 (digits n))
  where n = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

-- #9 --
-- What is the only pythagorean triple summing to one thousand?
problem9 =
  let (a, b, c) = tripleSummingTo 1000
  in a*b*c
  where
    tripleSummingTo target = head $ mapMaybe try pythagoreanTriples
      where
        try (a,b,c) =
          -- See if the sum divides the target, and skip if it doesn't divide evenly.
          let (n,d) = divMod target (a+b+c)
          in
            case d of
              0 -> Just (a*n, b*n, c*n)
              _ -> Nothing
