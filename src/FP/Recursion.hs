module FP.Recursion (gcd', fib, pascalTriangle) where

import Data.List (transpose)

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


---------

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = [1] : iter 1 [[1, 1]]
    where
        iter :: Int -> [[Int]] -> [[Int]]
        iter rowCount acc@(prevRow: xs) = if rowCount == n then acc else
            iter (rowCount + 1) [prevRow, makeRow prevRow []]
                where
                    makeRow :: [Int] -> [Int] -> [Int]
                    makeRow [] acc = acc
                    makeRow (x:xs) acc = makeRow xs ([x + (take 1 xs)] ++ acc)
