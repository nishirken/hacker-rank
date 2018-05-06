module FP.Intro (exponential, areaUnderCurves) where

truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral $ round $ x * t) / t
    where t = 10 ^ n

exponential :: Double -> Double
exponential 0.00 = 1.00
exponential n =
    truncate' (foldr (\x acc -> acc + (n ** x) / (product [1..x])) 1.00 [1..9]) 4

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

