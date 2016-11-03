-- Find a, b, c such that
-- a^2 + b^2 == c^2 and a + b + c == 1000 .
-- Substitute x for 1000, and , c = x - a - b, we get
-- b == (x^2 - 2*x*a) / (2*x - 2*b) .
-- Find a < x such that b is an integer.

import Data.Maybe (catMaybes)

findTripleSummingTo x = (a, b, c)
  where
    (a, b) = head $ catMaybes $ map try [1..x]
    c = x - a - b
    numer a = x^2 - 2*x*a
    denom a = 2*x - 2*a
    try a
      | denom a <= 0  = Nothing
      | otherwise  =
        let (b, r) = numer a `divMod` denom a
         in if r == 0 then Just (a, b) else Nothing

main = do
  let (a, b, c) = findTripleSummingTo 1000
  print $ a*b*c
