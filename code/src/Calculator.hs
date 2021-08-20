{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Calculator where

  
import Text.Parsec.Expr.Math
import Control.Monad.State.Lazy ( join, execState )
import Data.Bifunctor ( bimap, second, first )
import Data.Bits
import Data.List ( sort )
import System.TimeIt
import System.IO

--import Design.RCA
import DesignTools

import Cell.Cell
import Cell.Charge

import BistableEngine.State
import BistableEngine.Run
import BistableEngine.Input
import BistableEngine.IO


toBinary :: Int -> [Int]
toBinary = \case 0 -> [0] ; n -> toBinHelp n
  where toBinHelp = \case 0 -> [] ; n -> toBinHelp ( n `div` 2 ) ++ if even n then [0] else [1]

addLeadingZeroes :: [Int] -> [Int]
addLeadingZeroes bs = let l = length bs in replicate ( abs ( nextPowerOf2 l - maximum [ 2 , l ] ) ) 0 ++ bs
  where nextPowerOf2 n = if n .&. ( n - 1 ) == 0 then n else 2 * nextPowerOf2 ( n `div` 2 + n `mod` 2 )

parseExpr :: IO ( Either String (Int,Int) )
parseExpr = putStr "\nEnter expression: " >> hFlush stdout >> ( \str -> case parse str of
    Right ( Add ( Num a ) ( Num b ) ) -> Right ( round ( a :: Double ) , round ( b :: Double ) )
    _                                 -> Left str ) <$> getLine

numsToInput :: [Cell] -> (Int,Int) -> ( Input , [Cell] )
numsToInput cs ns = let (as,bs) = join bimap ( addLeadingZeroes . toBinary ) ns
                        (as',bs') = let (la,lb) = ( length as , length bs )
                                    in if la == lb then (as,bs)
                                       else ( if la < lb then first
                                              else second ) ( replicate ( abs ( la - lb ) ) 0 ++ ) (as,bs)
                        ce = stackNTimes ( maximum [ 0 , popCount ( length as' - 1 ) - 1 ] ) (2,2) cs
                        inps = sort $ filter isInput ce
                        inpsA = zip ( filter ( ( == "A" ) . label ) inps ) ( binaryToCharge as' )
                        inpsB = zip ( filter ( ( == "B" ) . label ) inps ) ( binaryToCharge bs' )
                    in (,ce) . ( : inpsA ++ inpsB ) . (,Negative) . head $ filter ( ( == "Cin" ) . label ) inps
  where binaryToCharge = reverse . map ( \b -> toEnum ( 2 * b - 1 ) )


outputToNum :: [Cell] -> Int
outputToNum = fromBinary 0 . map ( \c -> round ( pol c ) :: Int )
                               . ( (++) . filter ( ( == "Sum"  ) . label ) <*> filter ( ( == "Cout" ) . label ) )
  where fromBinary n = \case [] -> 0 ; (x:xs) -> x * 2 ^ ( n :: Int ) + fromBinary ( n + 1 ) xs

getOutput :: Integer -> ( Input , [Cell] ) -> Int
getOutput i (inps,ce) = outputToNum . head . parseOutput . outputs $ execState ( simulate i )
                        defaultState { cellEnv = ce , inputs = parseInputs ( filter isInput ce ) [inps] }


calculate :: Integer -> [Cell] -> IO ()
calculate i ce = parseExpr >>= ( \case
    Left str  -> if str == "exit" then pure () else calculate i ce
    Right inp -> ( timeIt . print $ getOutput i inp ) >> calculate i ce ) . ( numsToInput ce <$> )
    
calculateCE :: Integer -> [Cell] -> IO ()
calculateCE i ce = parseExpr >>= ( \case
    Left str  -> if str == "exit" then pure () else calculateCE i ce
    Right inp -> runSimVisual i ( const [ fst inp ] ) ( snd inp )
                 >> ( timeIt . print $ getOutput i inp ) >> calculateCE i ce ) . ( numsToInput ce <$> )
                 
calculateIO :: Integer -> [Cell] -> IO ()
calculateIO i ce = parseExpr >>= ( \case
    Left str  -> if str == "exit" then pure () else calculateIO i ce
    Right inp -> runSimPretty i ( const [ fst inp ] ) ( snd inp )
                 >> ( timeIt . print $ getOutput i inp ) >> calculateIO i ce ) . ( numsToInput ce <$> )