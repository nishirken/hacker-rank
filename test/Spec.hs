import FP.IntroSpec (introSpec)
import FP.AdHocSpec (adHocSpec)
import FP.RecursionSpec (recursionSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    introSpec
--    adHocSpec
    recursionSpec
