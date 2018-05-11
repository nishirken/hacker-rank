module FP.Recursion (gcd', fib, pascalTriangle, makeRow, rowToString, sierpinskiTriangle) where

import Data.List (intercalate)

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

makeRow :: [Int] -> [Int]
makeRow prevRow = 1 : zipWith (+) prevRow (tail prevRow) ++ [1]

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = [1] : iter 2 [[1, 1]]
    where
        iter :: Int -> [[Int]] -> [[Int]]
        iter rowCount acc = if rowCount == n then acc else
            iter (rowCount + 1) (acc ++ [makeRow $ last acc])

rowToString :: [Int] -> String
rowToString [] = ""
rowToString row = intercalate " " $ map show row

---------

sierpinskiTriangle :: Int -> String
sierpinskiTriangle _ = ""
