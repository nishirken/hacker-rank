module FP.RecursionSpec (recursionSpec) where

import Test.Hspec (SpecWith, describe, context, it, shouldBe)
import FP.Recursion (
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
    , nub'
    , stringCompression
    , sumsOfPowers
    )

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

    context "Filter list by repeated elements" $ do
        it "first case" $ repeatedFilter 2 [4, 5, 2, 5, 4, 3, 1, 3, 4] `shouldBe` [4, 5, 3]
        it "second case" $ repeatedFilter 4 [4, 5, 2, 5, 4, 3, 1, 3, 4] `shouldBe` [-1]
        it "third case" $ repeatedFilter 2 [5, 4, 3, 2, 1, 1, 2, 3, 4, 5] `shouldBe` [5, 4, 3, 2, 1]
        it "fourth case" $
            repeatedFilter 11 [6,10,3,5,8,4,3,3,5,4,4,5,5,6,2,3,9,7,6,4,3,5,3,10,7,2,6,3,9,7,9,7,2,10,3,6,5,2,8,8,3,3,2,6,10,6,6,6,6,9,3,9,3,7,8,9,3,3,4,7,7,5,8,9,4,8,7,3,7,7,2,5,5,5,10,10]
                `shouldBe` [3]
        it "fifth case" $
            repeatedFilter 1 [8,9,7,6,2,2,3,4,3,9,9,9,9,8,8,4,3,7,2,3,4]
                `shouldBe` [8,9,7,6,2,3,4]
        it "sixth case" $
            repeatedFilter 9 [3,6,2,10,3,5,5,6,8,5,8,2,3,9,10,3,7,3,5,3,4,7,6,8,10,6,5,7,6,2,6,2,8,4,7,8,10,6,7,6,5,4,4,6,2,5,7,3,6,7,2,8,6,8,4,2,3,7,4,2,3,6,10,9,9,2,7,10,6,8,9,2,4,9,10,8,8,3,6,8,5,7,8,4]
                `shouldBe` [3,6,2,8,7]

    context "Prefix Compression" $ do
        it "first case" $ prefixCompression "abcdefpr" "abcpqr" `shouldBe` ["3 abc", "5 defpr", "3 pqr"]
        it "second case" $ prefixCompression "kitkat" "kit" `shouldBe` ["3 kit", "3 kat", "0"]
        it "third case" $ prefixCompression "puppy" "puppy" `shouldBe` ["5 puppy", "0", "0"]

    context "nub implementaion" $ do
        it "first case" $ nub' "accabb" `shouldBe` "acb"
        it "second case" $ nub' "abc" `shouldBe` "abc"
        it "third case" $ nub' "pprrqq" `shouldBe` "prq"

    context "String Compression" $ do
        it "first case" $ stringCompression "abcaaabbb" `shouldBe` "abca3b3"
        it "second case" $ stringCompression "abcd" `shouldBe` "abcd"
        it "third case" $ stringCompression "aaabaaaaccaaaaba" `shouldBe` "a3ba4c2a4ba"

    context "Sums of Powers" $ do
        it "first case" $ sumsOfPowers 100 2 `shouldBe` 3
        it "second case" $ sumsOfPowers 100 3 `shouldBe` 1
        it "third case" $ sumsOfPowers 10 2 `shouldBe` 1
