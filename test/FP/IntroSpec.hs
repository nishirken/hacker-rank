module FP.IntroSpec (introSpec) where

import Test.Hspec (SpecWith, describe, context, it, shouldBe)
import FP.Intro (exponential, areaUnderCurves)

introSpec :: SpecWith ()
introSpec = describe "Intro" $ do
    context "exponential" $ do
        it "works with 0.00" $ exponential 0.00 `shouldBe` 1.0000
        it "works with 1.00" $ exponential 1.00 `shouldBe` 2.7183
        it "works with -0.5000" $ exponential (-0.5000) `shouldBe` 0.6065
        it "works with 0.5000" $ exponential 0.5000 `shouldBe` 1.6487
        it "works with 5.0000" $ exponential 5.0000 `shouldBe` 143.6895
        it "works with 20.0000" $ exponential 20.0000 `shouldBe` 2423600.1887

    context "areaUnderCurves" $
        it "works" $ areaUnderCurves 1 4 [1..5] [6..10] `shouldBe` [2435300.3, 26172951168898.6]