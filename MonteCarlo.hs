module MonteCarlo
    (
    ) where

import           Data.Random.Normal

data Option = Option { optionType :: String
                     , stock      :: Double
                     , strike     :: Double
                     , riskFree   :: Double
                     , time       :: Double
                     , vol        :: Double
                     }

payoffCall
    :: Double
    -> Double
    -> Double
payoffCall
    stock
    strike = maximum [stock - strike, 0]

payoffPut
    :: Double
    -> Double
    -> Double
payoffPut
    stock
    strike = maximum [strike - stock, 0]

epsilon :: IO Double
epsilon =   do
            sample <- normalIO
            return sample
