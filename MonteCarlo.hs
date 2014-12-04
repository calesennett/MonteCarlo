module MonteCarlo
    ( payoffCall
    , payoffPut
    , genPricePath
    , pricePaths
    , main
    , Option (..)
    , deltaT
    ) where

import           Data.Random.Normal
import           Math.Statistics

data Option = Option { optionType :: String
                     , stock      :: Double
                     , strike     :: Double
                     , time       :: Double
                     , vol        :: Double
                     , drift      :: Double
                     }
deltaT
    :: Option
    -> Double
deltaT
    option = (time option) / 365

main =  do
        let option  = Option { optionType="call"
                             , stock=31.25
                             , strike=22.75
                             , time=1.0
                             , vol=0.5
                             , drift=0.1
                             }
            trials  = 5000
        putStrLn "Running simulation..."
        allPaths <- sequence $ pricePaths option (stock option) trials
        let averages = map mean allPaths
            payoff  = mean $ calcPayoffs option averages
            discounted = payoff / (1.07)**(time option)
        print discounted

calcPayoffs
    :: Option
    -> [Double]
    -> [Double]
calcPayoffs
    option
    averages
    | optionType o == "call" = map (payoffCall $ strike o) averages
    | otherwise              = map (payoffPut $ strike o) averages
        where o = option

pricePaths
    :: Option
    -> Double
    -> Double
    -> [IO [Double]]
pricePaths
    option
    stock
    trials
    | trials == 0 = []
    | otherwise   = do
                    path <- [(genPricePath option stock ((deltaT option) * 365^2))]
                    paths <- [path] ++ (pricePaths option stock (trials - 1))
                    return paths

genPricePath
    :: Option
    -> Double
    -> Double
    -> IO [Double]
genPricePath
    option
    stock
    points
    | points <= 0 = return []
    | otherwise   = do
                    e <- epsilon
                    nextStep <- genPricePath option (nextPrice option stock e) (points - 1)
                    let path = (stock:nextStep)
                    return path

nextPrice
    :: Option
    -> Double
    -> Double
    -> Double
nextPrice
    option
    stock
    e = stock + (drift o) * stock * (deltaT o) + (vol o) * stock * e * sqrt (deltaT o)
        where o = option

payoffCall
    :: Double
    -> Double
    -> Double
payoffCall
    strike
    stock = maximum [stock - strike, 0]

payoffPut
    :: Double
    -> Double
    -> Double
payoffPut
    strike
    stock = maximum [strike - stock, 0]

epsilon :: IO Double
epsilon =   do
            sample <- normalIO
            return sample
