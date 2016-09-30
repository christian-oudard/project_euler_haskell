-- The first prime-free region of size 10 is from 200 - 209.
-- What is the first prime-free region of size 100?

import Utility (primes)

primesByRegion k = f regions primes
  where
    regions = [k, 2*k ..]
    f rs ps =
      let
        (d:rs') = rs
        (cur, ps') = span (<d) ps
      in cur : f rs' ps'

main = do
  let (s, _) = head.dropWhile (not.null.snd) $ zip [0..] $ primesByRegion 100
  putStrLn $ show (s*100) ++ " - " ++ show ((s+1)*100 - 1)
