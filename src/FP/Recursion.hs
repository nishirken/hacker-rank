module FP.Recursion (
    gcd'
    , fib
    , pascalTriangle
    , makeRow
    , rowToString
    , sierpinskiTriangle
    , mergeStrings
    , swapChars
    , repeatedFilter
    , prefixCompression
    ) where

import Data.List (intercalate, null, sortBy, groupBy, (\\))

---------- Computing the GCD

gcd' :: Integral a => a -> a -> a
gcd' 0 x = x
gcd' x 0 = x
gcd' x y = if (rem x y) == 0 then y else gcd' y (rem x y)

---------- Fibonacci Numbers

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = last $ take n $ fibStream
    where fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

--------- Pascal's Triangle

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

--------- Functions and Fractals: Sierpinski triangles

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

--------- String Mingling

mergeStrings :: String -> String -> String
mergeStrings "" b = b
mergeStrings a "" = a
mergeStrings a b = concat $ zipWith (\x y -> x : [y]) a b

--------- String-o-Permute

swapChars :: String -> String
swapChars "" = ""
swapChars (x:y:xs) = y : x : (swapChars xs)

--------- Filter Elements

repeatedFilter :: Int -> [Int] -> [Int]
repeatedFilter _ [] = []
repeatedFilter n list =
    if null res then [-1] else res
        where res =
                map snd
                    $ sortBy (\(a, _) (b, _) -> compare a b)
                    $ map head
                    $ filter (\x -> length x >= n)
                    $ groupBy (\(_, a) (_, b) -> a == b)
                    $ sortBy (\(_, a) (_, b) -> compare a b) (zip [(0 :: Int)..] list)

---------- Prefix Compression

prefixCompression :: String -> String -> [String]
prefixCompression x y =
    [(len prefix) ++ prefix, (len firstSuffix) ++ firstSuffix, (len secondSuffix) ++ secondSuffix]
        where
            pairs = zip x y
            len str = if (not . null) str then (show $ length str) ++ " " else (show $ length str)
            prefix = map fst $ takeWhile (\(a, b) -> a == b) pairs
            firstSuffix = x \\ prefix
            secondSuffix = y \\ prefix
