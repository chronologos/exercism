module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n | n > 0 = Just (collatzHelper n)
collatz n = Nothing

collatzHelper :: Integer -> Integer
collatzHelper n | n == 1 = 0
                | n `mod` 2 == 0 = 1 + collatzHelper (n `div` 2)
                | n `mod` 2 /= 0 = 1 + collatzHelper (3*n+1)
