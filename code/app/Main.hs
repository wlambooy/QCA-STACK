module Main where

import Calculator

import BistableEngine.Input
import BistableEngine.Run
import BistableEngine.IO
import DesignTools
import System.TimeIt (timeIt)
import Calculator
import Design.RCA
import Cell.Cell

import InputFile
import System.IO

--o = outputCell { loc = (1,0,0), phase = clock 1 }
--c1 = clock1Cell { loc = (0,0,0) }
--c2 = clock0Cell { loc = (-1,0,0) }
--c3 = inputCell { loc = (-2,0,0) }
--c4 = clock0Cell { loc = (0,1,0) }
--c5 = clock0Cell { loc = (0,2,0) }
--c6 = clock0Cell { loc = (0,-1,0) }
--c7 = clock0Cell { loc = (0,-2,0) }
--
--ce = [c1,c2,c3,c4,c5,c6,c7,o]


testCE :: [Cell] -> IO ()
testCE ce = do 
    hSetBuffering stdout NoBuffering 
    putStr "drop: "
    d <- ( read :: String -> Int ) <$> getLine
    putStr "take: "
    t <- ( read :: String -> Int ) <$> getLine
    putStr "phases: "
    p <- ( read :: String -> Integer ) <$> getLine
    putStr "iterations: "
    i <- ( read :: String -> Integer ) <$> getLine
--    putStr "outfile: "
--    o <- getLine
--    let outFile = "D:\\Willem\\Stack\\_RU\\BachelorThesis\\Haskell\\output\\" ++ o ++ ".txt"
--    runSimNPhasesWrite outFile i p ( take t . drop d . truthTable ) ce
    runSimNPhases i p ( take t . drop d . truthTable ) ce
    testCE ce

main :: IO ()
--main = do
----    ( Right ce ) <- parseFile "D:\\Willem\\Stack\\_RU\\BachelorThesis\\Haskell\\inputfiles\\testRCA.fqca"
--    calculateIO 30 rcaCellEnv
--main = do
--    ( Right ce ) <- parseFile "D:\\Willem\\Stack\\_RU\\BachelorThesis\\Haskell\\inputfiles\\2bit-AND.fqca"
--    let sce = stackNTimes 3 ce
--    timeIt $ runSimPretty 30 truthTable sce

main = do
--    ( Right ce ) <- parseFile "D:\\Willem\\Stack\\_RU\\BachelorThesis\\Haskell\\inputfiles\\4nMUX.fqca"
--    ( Right ce ) <- parseFile "D:\\Willem\\Stack\\_RU\\BachelorThesis\\Haskell\\inputfiles\\test\\2MUXv5.fqca"
--    let cee = stackTreeDesign 2 . stackTreeDesign 1 $ ce
    hSetBuffering stdout NoBuffering 
    putStr "iterations: "
    i <- ( read :: String -> Integer ) <$> getLine
    calculate i rcaCellEnv
--    calculateCE 30 ce
--    testCE cee
    
--    let aa = stackTreeDesign 1 ce
----    timeIt $ showCellEnv 1 aa
--    let bb = stackTreeDesign 2 aa
----    timeIt $ showCellEnv 1 bb
--    let cc = stackTreeDesign 3 bb
----    timeIt $ showCellEnv 1 cc
--    let dd = stackTreeDesign 4 cc
----    timeIt $ showCellEnv 1 dd
--    timeIt $ showCEClocks dd
----    runSimPretty 20 truthTable ce

