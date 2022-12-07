{-# LANGUAGE LambdaCase #-}

module BistableEngine.Run where


import Cell.Cell
import Cell.Phase

import DesignTools

import BistableEngine.State
import BistableEngine.Engine
import Cell.Input
import Cell.IO

import Control.Monad.State

-- | Thesis V1.1 - Section 3.2.2
simulate :: Integer -> State SimState ()
simulate maxIters = setInputCells >>
          doIterations maxIters >>= handleDubiousCells maxIters >>
                     addOutputs >> isFinished >>= flip unless ( nextPhase >> simulate maxIters )

-- | Thesis V1.1 - Section 3.2.4
runSimulation :: Integer -> [Input] -> [Cell] -> Output
runSimulation maxIters inps ce = outputs $ 
    execState ( simulate maxIters ) defaultState { cellEnv = ce 
                                                 , inputs  = parseInputs ( getInputs ce ) inps }

-- | Thesis V1.1 - Section 3.3.2
runSimPretty :: Integer -> ( [Cell] -> [Input] ) -> [Cell] -> IO ()
runSimPretty maxIters inputGen ce = let inCells = getInputs ce ; rawInps = inputGen inCells
                                    in prettyPrintIO rawInps . outputs $ execState ( simulate maxIters )
                                       defaultState { cellEnv = ce , inputs = parseInputs inCells rawInps }


simulatePrintCE :: Integer -> State SimState String
simulatePrintCE maxIters = setInputCells >>
                 doIterations maxIters >>= handleDubiousCells maxIters >>
                            addOutputs >> get >>= \st ->
                            let (cePrint,dims) = cellEnvToString False ( time st ) ( cellEnv st )
                                ceWithTime = "\ESC[0;97m Time: \ESC[1;94m" ++ show ( time st ) ++ "\n\n" ++ cePrint
                            in isFinished >>= \case
                 True  -> return ceWithTime
                 False -> nextPhase >> ( ( ceWithTime ++ lineBreak dims ) ++ ) <$> simulatePrintCE maxIters

-- | Thesis V1.1 - Section 3.3.2
runSimVisual :: Integer -> ( [Cell] -> [Input] ) -> [Cell] -> IO ()
runSimVisual maxIters inputGen ce = putStrLn $ evalState ( simulatePrintCE maxIters )
                                    defaultState { cellEnv = ce ,
                                                   inputs  = parseInputs <*> inputGen $ getInputs ce }


simulateN :: Integer -> Time -> State SimState ()
simulateN maxIters t = setInputCells >>
                    doIterations maxIters >>= handleDubiousCells maxIters >>
                               addOutputs >> get >>= \st ->
                                  unless ( time st >= t ) ( nextPhase >> simulateN maxIters ( t - 1 ) )

simulateNPrintCE :: Integer -> Time -> State SimState String
simulateNPrintCE maxIters t = setInputCells >>
                    doIterations maxIters >>= handleDubiousCells maxIters >>
                               addOutputs >> get >>= \st ->
                               let (cePrint,dims) = cellEnvToString False ( time st ) ( cellEnv st )
                                   ceWithTime = "\ESC[0;97m Time: \ESC[1;94m" ++ show ( time st )
                                                                              ++ "\n\n" ++ cePrint
                               in if time st >= t then return ceWithTime
                                  else nextPhase
                                       >> ( ( ceWithTime ++ lineBreak dims ) ++ ) <$> simulateNPrintCE maxIters t


runSimNPhases :: Integer -> Integer -> ( [Cell] -> [Input] ) -> [Cell] -> Output
runSimNPhases maxIters t inputGen ce = outputs $ execState ( simulateN maxIters t )
                                       defaultState { cellEnv = ce ,
                                                      inputs  = parseInputs <*> inputGen $ getInputs ce }
                                                      
runSimNPhasesCE :: Integer -> Integer -> ( [Cell] -> [Input] ) -> [Cell] -> IO ()
runSimNPhasesCE maxIters t inputGen ce = putStrLn $ evalState ( simulateNPrintCE maxIters t )
                                       defaultState { cellEnv = ce ,
                                                      inputs  = parseInputs <*> inputGen $ getInputs ce }

runSimNPhasesWrite :: String -> Integer -> Integer -> ( [Cell] -> [Input] ) -> [Cell] -> IO ()
runSimNPhasesWrite outFile maxIters t inputGen ce = writeFile outFile $ evalState ( simulateNPrintCE maxIters t )
                                      defaultState { cellEnv = ce ,
                                                     inputs  = parseInputs <*> inputGen $ getInputs ce }