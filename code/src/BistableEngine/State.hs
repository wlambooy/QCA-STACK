{-# LANGUAGE LambdaCase #-}

module BistableEngine.State where

import Cell.Cell
import Cell.Phase

import qualified Data.Map as M
import Control.Monad.State
import Data.List ( sort , sortBy , find )


data SimState = SS { cellEnv   :: [Cell]
                   , time      :: Time
                   , stability :: Stability
                   , inputs    :: [Input]
                   , inputBuf  :: Input
                   , outputs   :: Output
                   , finishLog :: (Int,Bool)
                   , kinkCache :: M.Map (Cell,Cell) Double }

defaultState :: SimState
defaultState = SS { cellEnv   = []
                  , time      = 0
                  , stability = False
                  , inputs    = []
                  , inputBuf  = []
                  , outputs   = []
                  , finishLog = (-1,False)
                  , kinkCache = M.empty }


instance Show SimState where
  show ss = "\nCellEnv:\n"  ++ foldr ( \c s -> show c ++ "\t\t" ++ show ( phase c ( time ss ) ) ++ s ) 
                                  "" ( cellEnv ss )
         ++ "\n\nTime: "    ++ show ( time ss )
         ++ "\nStability: " ++ show ( stability ss )
         ++ "\n\nInputs:"   ++ ( if ( null . inputs ) ss then "\n"
                                 else foldr ( \(ic,ch) ics -> identAndSpacer ic ++ show ch ++ ics )
                                       "\n" ( inputs ss !! fromIntegral ( time ss `mod` 4 ) ) )
         ++ "\n\nBuffer:"   ++ ( if ( null . inputBuf ) ss then "\n"
                                 else foldr ( \(ic,ch) ics -> identAndSpacer ic ++ show ch ++ ics )
                                       "\n" ( inputBuf ss ) )
         ++ "\nOutputs:\n"  ++ foldr ( \( (t,s) , outCells ) outs -> "\n Time: " ++ show t ++ "\t\t" 
                                                                   ++ "Stable: " ++ show s ++ "\n"
                            ++ foldr ( \o os -> show o ++ "\t\t" ++ show ( phase o t ) ++ os ) 
                                "\n" ( sort outCells ) ++ outs ) "\n" ( outputs ss )


getCell :: Cell -> State SimState ( Maybe Cell )
getCell c = gets $ find ( == c ) . cellEnv


incrTime :: State SimState Time
incrTime = modify ( \st -> st { time = time st + 1 } ) >> gets time


nextPhase :: State SimState Time
nextPhase = incrTime >>= (<$) <*> setHoldPols

getPhase :: Cell -> State SimState Phase
getPhase c = gets time >>= ( <$> getCell c ) . maybe ( error "getPhase: Cell not found" ) . flip phase 


getPol :: Cell -> State SimState Double
getPol c = maybe ( error "getPol: Cell not found" ) pol <$> getCell c

setPol :: Cell -> Double -> State SimState ()
setPol c p = get >>= \st -> getCell c >>= maybe ( error "setPol: Cell not found" ) 
                    ( \x -> put st { cellEnv = x { pol = p } : filter ( /= x ) ( cellEnv st ) } )


setHoldPols :: Time -> State SimState ()
setHoldPols t = modify $ \st -> st { cellEnv = map ( \c -> if phase c t /= Hold then c
                                                           else c { pol = signum $ pol c } ) $ cellEnv st }


getUndefPolCells :: State SimState [Cell]
getUndefPolCells = gets $ \st -> filter ( \c -> abs ( pol c ) < 0.05 && phase c ( time st ) == Switch ) $ cellEnv st

setStability :: Stability -> State SimState ()
setStability s = modify $ \st -> st { stability = s }


addOutputs :: State SimState ()
addOutputs = modify $ \st -> st { outputs = ( ( time st , stability st )
                                            , filter isOutput $ cellEnv st ) : outputs st }


checkCache :: Cell -> Cell -> State SimState ( Maybe Double )
checkCache c1 c2 = gets $ M.lookup ( min c1 c2 , max c1 c2) . kinkCache

setCache :: Cell -> Cell -> Double -> State SimState ()
setCache c1 c2 ke = modify $ \st -> st { kinkCache = M.insert ( min c1 c2 , max c1 c2 ) ke $ kinkCache st }


sortOnPropagation :: State SimState ()
sortOnPropagation = get >>= \st ->
     let env = cellEnv st ; t = time st
         hld = filter ( ( == Hold ) . flip phase t ) env
     in if null hld then pure ()
        else let swi = sortBy ( cmp hld ) $ filter ( ( == Switch ) . flip phase t ) env
             in put st { cellEnv = filter ( `notElem` swi ) env ++ swi }
  where cmp hld x y = case getZ x `compare` getZ y of
                        LT -> LT
                        EQ -> let drivers = filter ( ( getZ x == ) . getZ ) hld
                              in minDistance drivers x `compare` minDistance drivers y
                        GT -> GT


nInputsEmpty :: Int -> State SimState Bool
nInputsEmpty n = gets $ all null . take n . inputs

lastOutput :: State SimState Bool
lastOutput = get >>= \st -> let (n,b) = finishLog st in if b
    then case find ( \c -> phase c ( time st ) == Hold && number c >= n ) . concatMap snd $ outputs st of
           Nothing -> pure False
           Just c  -> ( number c == getMaxOutputNum ( cellEnv st ) ) <$ put st { finishLog = ( number c , True ) } 
    else nInputsEmpty 2 >>= ( False <$ ) . flip when ( put st { finishLog = (n,True) } )

isFinished :: State SimState Bool
isFinished = nInputsEmpty 1 >>= \case True -> lastOutput ; False -> pure False
