module Cell.Phase where

type Time = Integer
  
data Phase = Hold | Release | Relax | Switch
  deriving ( Show , Eq , Enum , Ord )


clock :: Int -> Time -> Phase
clock i = toEnum . ( `mod` 4 ) . subtract ( i + 1 ) . fromIntegral

incrClock :: Int -> ( Time -> Phase ) -> Time -> Phase
incrClock n f = f . subtract ( toInteger n )

decrClock :: Int -> ( Time -> Phase ) -> Time -> Phase
decrClock n f = f . ( + toInteger n )