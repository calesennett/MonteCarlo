module MonteCarlo.Test where

import           MonteCarlo hiding (main)
import           Test.Hspec

main =  hspec $ do
        describe "deltaT" $ do
            it "returns correct time step" $ do
                let o = Option { optionType="call"
                               , stock=31.25
                               , strike=22.75
                               , time=1.0
                               , vol=0.5
                               , drift=0.1
                               }
                deltaT o `shouldSatisfy` within 0.001 0.00273972602

        describe "payoffCall" $ do
            it "returns correct call payout" $ do
                let stock  = 31.25
                    strike = 29.00
                payoffCall strike stock `shouldBe` 2.25

            it "computes worthless payoff" $ do
                let stock  = 31.25
                    strike = 35.00
                payoffCall strike stock `shouldBe` 0.0

        describe "payoffPut" $ do
            it "returns correct put payout" $ do
                let stock  = 31.25
                    strike = 35.00
                payoffPut strike stock `shouldBe` 3.75

            it "computes worthless payoff" $ do
                let stock  = 31.25
                    strike = 30.00
                payoffPut strike stock `shouldBe` 0.0

        describe "genPricePath" $ do
            it "computes correct number of steps" $ do
                let o = Option { optionType="call"
                               , stock=31.25
                               , strike=22.75
                               , time=0.49315068493
                               , vol=0.5
                               , drift=0.1
                               }
                path <- genPricePath o (stock o) ((deltaT o) * 365^2)
                length path `shouldBe` 180

        describe "pricePaths" $ do
            it "returns the correct number of trials" $ do
                let o = Option { optionType="call"
                               , stock=31.25
                               , strike=22.75
                               , time=1.0
                               , vol=0.5
                               , drift=0.1
                               }
                    trials = 1000
                length (pricePaths o (stock o) trials) `shouldBe` 1000
within
    :: Double
    -> Double
    -> Double
    -> Bool
within
    tolerance
    expected
    actual = if lower <= actual && actual <= upper
             then True
             else False
             where lower = (expected - expected * tolerance)
                   upper = (expected + expected * tolerance)

