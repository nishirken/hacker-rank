module FP.Recursion (gcd', fib, pascalTriangle, makeRow, rowToString, sierpinskiTriangle, triangleView) where

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

triangleView :: Int -> Int -> [String]
triangleView rows columns =
    map rowView [1..rows]
        where
            rowView :: Int -> String
            rowView row = underscores ++ (replicate ones '1') ++ underscores
                where
                    ones = row * 2 - 1
                    underscores = replicate (round $ (fromIntegral (columns) - fromIntegral (ones)) / 2) '_'

underscoreView :: Int -> Int -> [String]
underscoreView rows columns = map (\x -> replicate columns '_') [1..rows]

makeTree :: Int -> Int -> Int -> [String]
makeTree 0 rows columns = triangleView rows columns
makeTree n rows columns =
    (++)
        (zipWith3 (\x y z -> x ++ y ++ z) underscoreField square underscoreField)
        (zipWith3 (\x y z -> x ++ y ++ z) square gap square)
        where
            square = (makeTree (n - 1) (round $ fromIntegral rows / 2) (floor $ fromIntegral columns / 2))
            underscoreField = underscoreView (round $ fromIntegral rows / 2) (round $ fromIntegral columns / 4)
            gap = underscoreView (round $ fromIntegral rows / 2) 1

sierpinskiTriangle :: Int -> String
sierpinskiTriangle iterationCount = concat $ map (++ "\n") $ makeTree iterationCount 32 63
