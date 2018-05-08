module FP.IntroSpec (introSpec) where

import Test.Hspec (SpecWith, describe, context, it, shouldBe)
import FP.Intro (exponential, areaUnderCurves, splitOn, makePairs, functionOrNot, polygonPerimeter)

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
        it "works" $ head (areaUnderCurves 1 4 [1..5] [6..10]) `shouldBe` 2435300.3

    context "functionsOrNot" $ do
        it "splitOn function" $ splitOn "1\n2\n1 3\n6 7" '\n' `shouldBe` ["1", "2", "1 3", "6 7"]
        it "makePairs parse inputs and create Int pairs" $ do
            makePairs ["3", "1 3", "4 5", "7 1", "1", "2 4"] `shouldBe` [[(1, 3), (4, 5), (7, 1)], [(2, 4)]]
            makePairs [] `shouldBe` []
            makePairs [""] `shouldBe` []
        it "functionOrNot with yes and no" $
            functionOrNot [(zip [1..101] [300..401]), (zip [1,1..100] [1..100])] `shouldBe` ["YES", "NO"]
        it "functionOrNot with only yes" $ functionOrNot [[(1, 2), (3, 4)], [(1, 1), (1, 1)]] `shouldBe` ["YES", "YES"]
        it "functionOrNot with only no" $ functionOrNot [[(2, 2), (2, 4)], [(1, 1), (1, 4)]] `shouldBe` ["NO", "NO"]
        it "works together" $
            ((functionOrNot . makePairs) $
            splitOn "100\n\
                \1 499\n\
                \2 498\n\
                \3 497\n\
                \4 496\n\
                \5 495\n\
                \6 494\n\
                \7 493\n\
                \8 492\n\
                \9 491\n\
                \10 490\n\
                \11 489\n\
                \12 488\n\
                \13 487\n\
                \14 486\n\
                \15 485\n\
                \16 484\n\
                \17 483\n\
                \18 482\n\
                \19 481\n\
                \20 480\n\
                \21 479\n\
                \22 478\n\
                \23 477\n\
                \24 476\n\
                \25 475\n\
                \26 474\n\
                \27 473\n\
                \28 472\n\
                \29 471\n\
                \30 470\n\
                \31 469\n\
                \32 468\n\
                \33 467\n\
                \34 466\n\
                \35 465\n\
                \36 464\n\
                \37 463\n\
                \38 462\n\
                \39 461\n\
                \40 460\n\
                \41 459\n\
                \42 458\n\
                \43 457\n\
                \44 456\n\
                \45 455\n\
                \46 454\n\
                \47 453\n\
                \48 452\n\
                \49 451\n\
                \50 450\n\
                \51 449\n\
                \52 448\n\
                \53 447\n\
                \54 446\n\
                \55 445\n\
                \56 444\n\
                \57 443\n\
                \58 442\n\
                \59 441\n\
                \60 440\n\
                \61 439\n\
                \62 438\n\
                \63 437\n\
                \64 436\n\
                \65 435\n\
                \66 434\n\
                \67 433\n\
                \68 432\n\
                \69 431\n\
                \70 430\n\
                \71 429\n\
                \72 428\n\
                \73 427\n\
                \74 426\n\
                \75 425\n\
                \76 424\n\
                \77 423\n\
                \78 422\n\
                \79 421\n\
                \80 420\n\
                \81 419\n\
                \82 418\n\
                \83 417\n\
                \84 416\n\
                \85 415\n\
                \86 414\n\
                \87 413\n\
                \88 412\n\
                \89 411\n\
                \90 410\n\
                \91 409\n\
                \92 408\n\
                \93 407\n\
                \94 406\n\
                \95 405\n\
                \96 404\n\
                \97 403\n\
                \98 402\n\
                \99 401\n\
                \100 400" '\n') `shouldBe` ["YES"]

    context "polygon perimeter" $
        it "works" $
            polygonPerimeter [[(0, 0), (0, 1), (1, 1), (1, 0)],
                [(1043, 770), (551, 990), (681, 463)],
                [(458, 695), (621, 483), (877, 469), (1035, 636), (1061, 825), (875, 1023), (645, 1033), (485, 853)]]
                `shouldBe` [4, 1556.394903, 1847.480551]

