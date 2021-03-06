{-# LANGUAGE LambdaCase #-}

module BistableEngine.Engine where

  
import Cell.Cell
import Cell.Phase
import BistableEngine.State

import Control.Monad
import Control.Monad.State
import Data.Matrix


radiusOfEffect :: Double
--radiusOfEffect = 65.00
radiusOfEffect = 5.0

convergenceTolerance :: Double
convergenceTolerance = 1e-8


-- | Thesis V1.1 - Section 3.1.1
calcKinkEnergy :: Cell -> Cell -> Double
calcKinkEnergy c1 c2 = sum . toList . flatten .
    mapPos ( \(_,col1) a ->
      mapPos ( \(_,col2) b ->
        fromIntegral ( ( -2 * col1 + 3 ) * ( 2 * col2 - 3 ) ) 
        / absQDotDistance a b
      ) $ getQDots c2
    ) $ getQDots c1

getOrCalcKE :: Cell -> Cell -> State SimState Double
getOrCalcKE c1 c2 = checkCache c1 c2 >>= maybe ( (<$) <*> setCache c1 c2 $ calcKinkEnergy c1 c2 ) pure

-- | Thesis V1.1 - Section 3.1.2 >>
getNeighbours :: Cell -> State SimState [Cell]
getNeighbours cell = gets $ filter ( \c -> absCellDistance cell c < radiusOfEffect && c /= cell ) . cellEnv

newPolarisation :: Cell -> State SimState Double
newPolarisation c = ( \x -> x / sqrt ( 1 + x ** 2 ) ) . sum . 
  ( ( zipWith (*) . map pol ) <*> map ( calcKinkEnergy c ) )
  <$> getNeighbours c
-- | <<
  

-- | Thesis V1.1 - Section 3.2.1 >>
tick :: Time -> Cell -> State SimState ()
tick t c
 | ( phase c t == Hold && pol c /= 0 ) || isInput c || phase c t == Relax = pure ()
 |   phase c t == Release = setPol c 0
 | otherwise = newPolarisation c >>= \newPol ->
               when ( abs ( newPol - pol c ) > convergenceTolerance ) ( setStability False )
               >> setPol c ( if phase c t == Hold then signum newPol else newPol )

tickAll :: State SimState Stability
tickAll = get >>= \st -> foldr ( (>>) . tick ( time st ) ) ( gets stability ) $ cellEnv st

doIterations :: Integer -> State SimState [Cell]
doIterations 0 = getDubiousCells
doIterations n = setStability True >> sortOnPropagation >> tickAll >>= \case
    True  -> getDubiousCells
    False -> doIterations $ n - 1
-- | <<

-- | Thesis V1.1 - Section 3.1.3
handleDubiousCells :: Integer -> [Cell] -> State SimState ()
handleDubiousCells maxIters uds = unless ( null uds ) $ do
    ce <- gets cellEnv
    modify $ \st -> st { cellEnv = filter ( `notElem` uds ) ce }
    t <- nextPhase          
    _ <- doIterations maxIters ; setHoldPols $ t + 1
    modify $ \st -> st { cellEnv = cellEnv st ++ uds }
    foldr ( (>>) . tick t . ( \c -> c { pol = 0 } ) ) ( pure () ) uds
    modify $ \st -> st { cellEnv = filter ( `elem` uds ) ( cellEnv st )
                                ++ filter ( `notElem` uds ) ce , time = time st - 1 }
