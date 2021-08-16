{-# LANGUAGE LambdaCase #-}

module Cell.Cell where


import Cell.Phase
import Cell.Charge

import Data.List ( maximumBy )
import Data.Matrix
import Control.Monad


type Pos = (Double,Double,Double)
type Stability = Bool

type Input = [(Cell,Charge)]
type Output = [ ( (Time,Stability) , [Cell] ) ]


data Cell = Cell { loc       :: Pos                    -- Absolute location of the cell on the grid
                 , label     :: String                 -- Label used for printing the cell
                 , pol       :: Double                 -- Polarity of the cell
                 , phase     :: Time -> Phase          -- Phase as a function of time
                 , isInput   :: Bool                   -- True iff the cell is an input cell
                 , isOutput  :: Bool                   -- True iff the cell is an output cell
                 , number    :: Int
                 , propagate :: Bool }

defaultCell :: Cell
defaultCell = Cell { loc       = (0,0,0)
                   , label     = ""
                   , pol       = 0
                   , phase     = clock 0
                   , isInput   = False
                   , isOutput  = False
                   , number    = -1
                   , propagate = False }


instance Eq Cell where
  c1 == c2 = loc c1 == loc c2

instance Ord Cell where
  compare c1 c2 = case label c1 `compare` label c2 of 
                    LT -> LT
                    GT -> GT
                    EQ -> case number c1 `compare` number c2 of
                            LT -> LT
                            GT -> GT
                            EQ -> loc c1 `compare` loc c2


instance Show Cell where
  show c = identAndSpacer c ++ if pol c /= ( fromInteger . round . pol ) c then show ( pol c )
                               else show ( round ( pol c ) :: Integer )

showLabel :: Cell -> String
showLabel c = label c ++ if number c < 0 || propagate c then "" else ( show . number ) c

identAndSpacer :: Cell -> String
identAndSpacer c = let (locOrLbl,t) = if null ( label c ) then ( show ( loc c ) , 20 ) else ( showLabel c , 12 )
                       t'           = if pol c < 0        then t - 1                   else t
                   in "\n " ++ locOrLbl ++ ":" ++ replicate ( t' - length locOrLbl - 2 ) ' '


getMaxOutputNum :: [Cell] -> Int
getMaxOutputNum = number . maximumBy ( ( . number ) . compare . number ) . filter isOutput


getX , getY , getZ :: Cell -> Double
getX c = let (x,_,_) = loc c in x
getY c = let (_,y,_) = loc c in y
getZ c = let (_,_,z) = loc c in z
  
setX , setY , setZ :: Double -> Cell -> Cell
setX x c = let (_,y,z) = loc c in c { loc = (x,y,z) }
setY y c = let (x,_,z) = loc c in c { loc = (x,y,z) }
setZ z c = let (x,y,_) = loc c in c { loc = (x,y,z) }


sortOnX , sortOnY , sortOnZ :: Cell -> Cell -> Ordering
sortOnZ = ( . getZ ) . compare . getZ
sortOnY = ( . getY ) . compare . getY
sortOnX = ( . getX ) . compare . getX

xyzFactor :: Floating a => [a]
--xyzFactor = [1,1,0.581214566722247681924037319514494021827]
xyzFactor = [1,1,0.581215]
--xyzFactor = [1,1,1]


posMath :: ( Double -> Double -> Double ) -> Pos -> Pos -> Pos
posMath op (x1,y1,z1) (x2,y2,z2) = let [x,y,z] = zipWith op [x1,y1,z1] [x2,y2,z2] in (x,y,z)
                                          
absDistance :: Pos -> Double
absDistance (x,y,z) = sqrt . sum . map ( join (*) ) $ zipWith (*) xyzFactor [x,y,z]

absCellDistance :: Cell -> Cell -> Double
absCellDistance c = absDistance . posMath (-) ( loc c ) . loc


qDotOffsets :: [Pos]
qDotOffsets = [ (  0.25 , -0.25 , 0 )
              , (  0.25 ,  0.25 , 0 )
              , ( -0.25 ,  0.25 , 0 )
              , ( -0.25 , -0.25 , 0 ) ]

getQDots :: Cell -> Matrix Pos
getQDots c = fromList 2 2 $ map ( posMath (+) $ loc c ) qDotOffsets

absQDotDistance :: Pos -> Pos -> Double
absQDotDistance = ( absDistance . ) . posMath (-)


minDistance :: [Cell] -> Cell -> Double
minDistance = \case [] -> const 0 ; xs -> flip ( ( minimum . ) . map . absCellDistance ) xs
