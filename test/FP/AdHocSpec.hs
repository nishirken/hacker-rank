module FP.AdHocSpec (adHocSpec) where

import Test.Hspec (SpecWith, describe, context, it, shouldBe)
import FP.AdHoc (hugeGCD)

adHocSpec :: SpecWith ()
adHocSpec = describe "Ad Hoc" $
    context "Huge GCD" $ do
        it "first case" $
            hugeGCD [[2, 2, 3, 3, 25], [8, 1, 6, 170]] `shouldBe` 60

        it "second case" $
            hugeGCD [[1, 2, 4, 8, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192], [1, 3, 9, 27, 81, 243, 729, 2187, 6561]]
                `shouldBe` 1

        it "third case" $
            hugeGCD [[2, 3, 5], [4, 5]] `shouldBe` 10
