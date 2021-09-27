{-# LANGUAGE LambdaCase #-}

module Main where

  
import Calculator
import InputFile

import BistableEngine.Input
import BistableEngine.Run
import BistableEngine.IO

import DesignTools
import Design.RCA

import Cell.Cell

import System.IO
import System.TimeIt (timeIt)


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
--    let outFile = "path" ++ o ++ ".txt"
--    runSimNPhasesWrite outFile i p ( take t . drop d . truthTable ) ce
    runSimNPhases i p ( take t . drop d . truthTable ) ce
    testCE ce

loadCE :: IO [Cell]
loadCE = do
    hSetBuffering stdout NoBuffering
    putStr "input file: "
    x <- getLine
    parseFile x >>= \case
      Left a   -> print a >> loadCE
      Right ce -> do
        putStr "stack linearly n times: "
        n <- ( read :: String -> Int ) <$> getLine
        if n == 0 then (>>) . showCEClocks <*> return $ ce else do
           putStr "clock offset: "
           a <- ( read :: String -> Int ) <$> getLine
           putStr "number offset: "
           b <- ( read :: String -> Int ) <$> getLine
           (>>) . showCEClocks <*> return $ stackNTimes n (a,b) ce

runTT :: [Cell] -> IO ()
runTT ce = do
    hSetBuffering stdout NoBuffering
    putStr "Run truth table?: "
    getLine >>= \case
      "y" -> do
        putStr "iterations: "
        i <- ( read :: String -> Integer ) <$> getLine
        runSimPretty i truthTable ce
      _   -> return ()

main :: IO ()
main = do
    loadCE >>= ( (>>) . runTT ) <*> testCE

--    hSetBuffering stdout NoBuffering
--    putStrLn "::   calculator with RCA v1   |   type \"exit\" to quit   ::"
--    putStr "iterations: "
--    i <- getLine
--    if i == "exit" then return ()
--    else calculate ( ( read :: String -> Integer ) i ) rcaCellEnv

--    ( Right ce ) <- parseFile "..\\input_files\\4nMUX.fqca"
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

