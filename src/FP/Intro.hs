module FP.Intro (exponential) where

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral $ round $ x * t) / t
    where t = 10 ^ n

exponential :: Double -> Double
exponential 0.00 = 1.00
exponential n =
    truncate' (foldr (\x acc -> acc + (n ** x) / (product [1..x])) 1.00 [1..9]) 4
