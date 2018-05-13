module FP.RecursionSpec (recursionSpec) where

import Test.Hspec (SpecWith, describe, context, it, shouldBe)
import FP.Recursion (gcd', fib, pascalTriangle, makeRow, rowToString, sierpinskiTriangle, mergeStrings, swapChars)

recursionSpec :: SpecWith ()
recursionSpec = describe "Recursion" $ do
    context "GCD" $ do
        it "first case" $ gcd' 1 5 `shouldBe` 1
        it "second case" $ gcd' 10 100 `shouldBe` 10
        it "third case" $ gcd' 22 131 `shouldBe` 1

    context "fibonacci" $ do
        it "first case" $ fib 3 `shouldBe` 1
        it "second case" $ fib 5 `shouldBe` 3

    context "Pascal triangle" $ do
        it "makeRow" $ makeRow [1, 3, 3, 1] `shouldBe` [1, 4, 6, 4, 1]
        it "rowToString" $ rowToString [1, 3, 3, 1] `shouldBe` "1 3 3 1"
        it "works completely" $ pascalTriangle 4 `shouldBe` [[1], [1, 1], [1, 2, 1], [1, 3, 3, 1]]

    context "Sierpinski triangle" $ do
        it "iteration 0" $ do
            expected <- readFile "./test/FP/SierpinskiTriangle/iteration0.txt"
            sierpinskiTriangle 0 `shouldBe` expected

        it "iteration 1" $ do
            expected <- readFile "./test/FP/SierpinskiTriangle/iteration1.txt"
            sierpinskiTriangle 1 `shouldBe` expected

        it "iteration 2" $ do
            expected <- readFile "./test/FP/SierpinskiTriangle/iteration2.txt"
            sierpinskiTriangle 2 `shouldBe` expected

        it "iteration 5" $ do
            expected <- readFile "./test/FP/SierpinskiTriangle/iteration5.txt"
            sierpinskiTriangle 5 `shouldBe` expected

    context "Merge strings" $ do
        it "first case" $ mergeStrings "abcde" "pqrst" `shouldBe` "apbqcrdset"
        it "second case" $ mergeStrings "hacker" "ranker" `shouldBe` "hraacnkkeerr"
        it "third case" $
            mergeStrings
            "hzeqwhgjvmrljpxtonobajvgjwjqpjmnxtlgbcyqbntpoorexu"
            "fjftcaukbqckpytznnnmisqncpjcgqcyqncezeymywxsfrcfek"
                `shouldBe`
                    "hfzjefqtwchagujkvbmqrclkjppyxttzonnnonbmaijsvqgnjcwpjjqcpgjqmcnyxqtnlcgebzceyyqmbynwtxpsoforrcefxeuk"

    context "Swap chars in string" $ do
        it "first case" $ swapChars "abcdpqrs" `shouldBe` "badcqpsr"
        it "second case" $ swapChars "az" `shouldBe` "za"

