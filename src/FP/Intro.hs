module FP.Intro (exponential, areaUnderCurves, splitOn, makePairs, functionOrNot, polygonPerimeter, polygonArea) where

import Data.List (transpose)

------------ Evaluating e^x

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral $ round $ x * t) / t
    where t = 10 ^ n

exponential :: Double -> Double
exponential 0.00 = 1.00
exponential n =
    truncate' (foldr (\x acc -> acc + (n ** x) / (product [1..x])) 1.00 [1..9]) 4

------------- Area Under Curves and Volume of Revolving a Curve

dX = 0.001

getPointers :: Int -> Int -> [Double]
getPointers l r =
    takeWhile (<= fromIntegral r) $ iterate (+ dX) (fromIntegral l + dX)

circArea :: Double -> Double
circArea r = r ^ 2 * pi

evalP :: [Int] -> [Int] -> Double -> Double
evalP a b x = sum $ zipWith (\a b -> (fromIntegral a) * x ** (fromIntegral b)) a b

areaUnderCurves :: Int -> Int -> [Int] -> [Int] -> [Double]
areaUnderCurves l r a b = map (\x -> truncate' x 1) [area, volume]
    where
        area = sum $ map((* dX) . evalP a b) $ getPointers l r
        volume = sum $ map((* dX) . circArea . evalP a b) $ getPointers l r

------------- Functions or Not?

splitOn :: String -> Char -> [String]
splitOn "" _ = []
splitOn inputData at =
    [(takeWhile (/= at) inputData)] ++ (splitOn (drop 1 $ dropWhile (/= at) inputData) at)

type Pairs = [[(Int, Int)]]
type Answer = String

makePairs :: [String] -> Pairs
makePairs [] = []
makePairs [""] = []
makePairs (x: xs) =
    [pairs] ++ (makePairs $ drop (readString x) xs)
        where
            readString x = (read :: String -> Int) x
            parsePair str =
                let stringPair = splitOn str ' ' in
                (readString $ head stringPair, readString $ last stringPair)
            pairs =
                map parsePair $ take (readString x) xs

functionOrNot :: Pairs -> [Answer]
functionOrNot [] = ["NO"]
functionOrNot pairs = map (answer . check) pairs
    where
        check x = all (\pair -> not $ hasSamePointPair pair x) x
        hasSamePointPair pair pairs = any (\x -> fst x == fst pair && snd x /= snd pair) pairs
        answer x = if x then "YES" else "NO"

-------------- Compute the Perimeter of a Polygon

vectorLength :: [(Int, Int)] -> Double
vectorLength coords =
    (sqrt . fromIntegral) $ (((fst secondPoint) - (fst firstPoint)) ^ 2) + (((snd secondPoint) - (snd firstPoint)) ^ 2)
        where
            firstPoint = head coords
            secondPoint = last coords

polygonPerimeter :: Pairs -> [Double]
polygonPerimeter [] = []
polygonPerimeter pairs = map perimeter pairs
    where
        perimeter pairs = truncate' (sum $ map vectorLength $ transpose ([pairs] ++ [(tail pairs) ++ (take 1 pairs)])) 6

-------------- Compute the Area of a Polygon

multiplyLists :: Num a => [a] -> [a] -> [a]
multiplyLists [] _ = []
multiplyLists _ [] = []
multiplyLists (x: xs) (y: ys) = [x * y] ++ (multiplyLists xs ys)

polygonArea :: Pairs -> [Double]
polygonArea [] = []
polygonArea pairs = map area pairs
    where
        area :: [(Int, Int)] -> Double
        area pairs = abs $
            ((fromIntegral $ sumOfProducts (init xList) (tail yList)) -
            (fromIntegral $ sumOfProducts (tail xList) (init yList))) / 2
                where
                    sumOfProducts :: [Int] -> [Int] -> Int
                    sumOfProducts x y = sum $ multiplyLists x y
                    xList :: [Int]
                    xList = fst $ unzip $ pairs ++ [head pairs]
                    yList :: [Int]
                    yList = snd $ unzip $ pairs ++ [head pairs]
