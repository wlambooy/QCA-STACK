module Cell.Charge where

import Data.Maybe ( fromJust )
import Data.Tuple ( swap )
  
data Charge = Negative | Neutral | Positive
  deriving Eq

instance Enum Charge where                        -- Enables arithmetic operations on charges
  fromEnum = fromJust . flip lookup table
  toEnum   = fromJust . flip lookup ( map swap table )

instance Show Charge where
  show = show . ( `div` 2 ) . ( + 1 ) . signum . fromEnum
  

table :: [(Charge,Int)]
table = [ ( Negative , -1 )
        , ( Neutral  ,  0 )
        , ( Positive ,  1 ) ]

toDouble :: Charge -> Double
toDouble = fromIntegral . fromEnum
