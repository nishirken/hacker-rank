module FP.Recursion (gcd', fib) where

gcd' :: Integral a => a -> a -> a
gcd' 0 x = x
gcd' x 0 = x
gcd' x y = if (rem x y) == 0 then y else gcd' y (rem x y)

----------

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = last $ take n $ fibStream
    where fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)
