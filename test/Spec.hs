import FP.IntroSpec (introSpec)
import FP.AdHocSpec (adHocSpec)
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    introSpec
--    adHocSpec
