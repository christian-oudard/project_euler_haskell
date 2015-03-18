main = print $ maximum (factorize 600851475143)

-- Try dividing n by a list of integers. Return a list whose product is n.
tryDiv :: Integer -> [Integer] -> [Integer]
tryDiv n [] = [n]
tryDiv n (x:xs)
    | (r == 0) = [x,q]
    | otherwise = tryDiv n xs
    where (q,r) = divMod n x

factorize :: Integer -> [Integer]
factorize n
    | (length divs == 1) = divs
    | otherwise = init divs ++ factorize (last divs)
    where
        divs = tryDiv n nums
        nums = 2:[3,5..n `div` 2]
