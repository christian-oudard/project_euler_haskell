main = print problem4

problem4 = [n | n <- nums, isPalindrome n]
    where
        nums = [x*y | x <- [999,998..900], y <- [999,998..x]]

isPalindrome n = reverse digits == digits
    where digits = splitDigits n

splitDigits n
    | n < base   = [n]
    | otherwise  = (splitDigits q) ++ [r]
        where
            (q,r) = n `divMod` base
            base = 10
            
