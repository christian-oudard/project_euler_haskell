import Utility (digits)

main = print problem4

problem4 = maximum [n | n <- nums, isPalindrome n]
    where
        nums = [x*y | x <- [999,998..900], y <- [999,998..x]]

isPalindrome n = reverse ds == ds
    where ds = digits n

