{-# LANGUAGE LambdaCase , TupleSections #-}

module BistableEngine.Input where

import Cell.Cell
import Cell.Charge
import Cell.Phase
import BistableEngine.State

import Control.Monad.State
import Data.List ( groupBy , sortBy )


-- | Thesis V1.1 - Section 3.2.3


truthTable :: [Cell] -> [Input]
truthTable [] = [[]]
truthTable (c:cs) = [ t:ts | t <- truth c , ts <- truthTable cs ]
  where truth x = [ (x,ch) | ch <- [Negative,Positive] ]


parseInputs :: [Cell] -> [Input] -> [Input]
parseInputs inCells = map rmvDummies . groupBy eqPhase .
                                       sortBy cmpPhase . 
                          addDummies . concat . ( map (,Neutral) ( offsetBuf inCells ) : )
  where
--    offsetInputs = dropWhile ( ( /= Switch ) . flip phase 0 ) . dropWhile ( ( == Switch ) . flip phase 0 )
    offsetInputs = dropWhile ( \c -> phase c 0 /= Switch ) . dropWhile ( \c -> phase c 0 == Switch )
    offsetBuf = \case [] -> [] ; inps -> (++) <*> offsetBuf $ offsetInputs inps
    eqPhase  =        ( . ( flip phase 0 . fst ) ) . (==)    . flip phase 0 . fst
    cmpPhase = flip $ ( . ( flip phase 0 . fst ) ) . compare . flip phase 0 . fst
    addDummies = ( ++ map ( \n -> ( defaultCell { number = -2 , phase = clock n } , Neutral ) ) [ 0 .. 4 ] )
    rmvDummies = filter ( ( /= -2 ) . number . fst )


setInputBuf :: Input -> State SimState ()
setInputBuf inp = modify ( \st -> st { inputBuf = inp } )

addToInputBuf :: (Cell,Charge) -> State SimState Bool
addToInputBuf (c,ch) = gets inputBuf >>= \ib -> if elem c $ map fst ib then pure False
                                                else True <$ setInputBuf ( ib ++ [(c, ch)] )

rmvFromInputBuf :: Cell -> State SimState ()
rmvFromInputBuf c = gets inputBuf >>= \ib -> setInputBuf [ x | x <- ib , fst x /= c ]

setFromInputBuf :: State SimState ()
setFromInputBuf = gets inputBuf >>= foldr ( \(c,ch) ss -> getPhase c >>= \case
        Switch  -> setPol c ( toDouble ch )        >> ss
        Release -> setPol c 0 >> rmvFromInputBuf c >> ss
        _       ->                                    ss ) ( pure () )


getInput :: State SimState Input
getInput = gets $ (!!) . inputs <*> ( fromEnum . ( `mod` 4 ) . time )

replaceInput :: Input -> State SimState ()
replaceInput newInp = modify ( \st -> let p = toEnum . fromIntegral . ( `mod` 4 ) $ time st
                                          [i1,i2,i3,i4] = inputs st in st { inputs = case p of
        Hold    -> [ newInp , i2 , i3 , i4 ]
        Release -> [ i1 , newInp , i3 , i4 ]
        Relax   -> [ i1 , i2 , newInp , i4 ]
        Switch  -> [ i1 , i2 , i3 , newInp ] } )

setInputCells :: State SimState ()
setInputCells = getInput >>= foldr ( \x ss -> addToInputBuf x >>= \b -> if b then ss else (:) x <$> ss ) ( pure [] )
                         >>= ( >> setFromInputBuf ) . replaceInput
