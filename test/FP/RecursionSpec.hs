module FP.RecursionSpec (recursionSpec) where

import Test.Hspec (SpecWith, describe, context, it, shouldBe)
import FP.Recursion (gcd', fib)

recursionSpec :: SpecWith ()
recursionSpec = describe "Recursion" $ do
    context "GCD" $ do
        it "first case" $ gcd' 1 5 `shouldBe` 1
        it "second case" $ gcd' 10 100 `shouldBe` 10
        it "third case" $ gcd' 22 131 `shouldBe` 1
    context "fibonacci" $ do
        it "first case" $ fib 3 `shouldBe` 1
        it "second case" $ fib 5 `shouldBe` 3
