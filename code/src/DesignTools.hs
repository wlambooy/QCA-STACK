{-# LANGUAGE TupleSections #-}

module DesignTools  where


import Cell.Cell
import Cell.Charge
import Cell.Phase

import Data.List ( sortBy )
import Data.Maybe ( mapMaybe )
import Control.Monad
import Data.Bifunctor ( bimap , first )
import Control.Arrow ( (***) )

inputCell , outputCell :: Cell
inputCell  = defaultCell { isInput  = True }
outputCell = defaultCell { isOutput = True }

clock0Cell , clock1Cell , clock2Cell , clock3Cell :: Cell
clock0Cell = defaultCell
clock1Cell = defaultCell { phase = clock 1 }
clock2Cell = defaultCell { phase = clock 2 }
clock3Cell = defaultCell { phase = clock 3 }

positiveCell , negativeCell :: Cell
positiveCell = defaultCell { pol = toDouble Positive , phase = const Hold }
negativeCell = defaultCell { pol = toDouble Negative , phase = const Hold }

getInputs :: [Cell] -> [Cell]
getInputs = sortBy ( ( . number ) . compare . number  ) . filter isInput

-- | Thesis V1.1 - Section 4.2.3
stackDesign :: Int -> (Int,Int) -> [Cell] -> [Cell]
stackDesign n (a,b) ce = map ( \c -> if not ( propagate c && isOutput c ) then c
                                     else c { isOutput = False , propagate = False , label = "" } ) ce
                      ++ mapMaybe ( \c -> let num = number c
                                              c'  = c { loc   = posMath (+) (0,0,maxHeight) $ loc c
                                                      , phase = if phase c 0 == phase c 1 then const Hold
                                                                else incrClock ( a * n ) $ phase c }
                                          in if propagate c' && isInput c' then Nothing
                                             else Just $ if num < 0 then c'
                                                         else c' { number = num + ( b * n ) } ) ce
  where maxHeight = maximum $ map getZ ce

stackNTimes :: Int -> (Int,Int) -> [Cell] -> [Cell]
stackNTimes 0 _ = id
stackNTimes n t = stackDesign ( 2 ^ ( n - 1 ) ) t . stackNTimes ( n - 1 ) t



-- | Thesis V1.1 - Section 4.4.2 >>
stackTreeDesign :: Int -> [Cell] -> [Cell]
stackTreeDesign 0 ce = ce
stackTreeDesign n ce = inps ce ++ ( (++) . filter ( not . propagate ) <*> ( \(w,m) -> w ++ root m ce )
                                  . wires ( getNonSelInps ce ) . map ( \c -> c { propagate = False } )
                                  . filter propagate ) ( newLeaf ce )
  where
    sx = 4 * 2 ^ n - 1 ; rootOffset = 2 ^ ( n + 1 )
--    sMin (-1) = 1 ; sMin 0 = 2 ; sMin n' = 2 ^ ( n' + 1 ) + sMin ( n' - 2 )                      -- linear non-homogeneous recurrence relation
    sMin n' = ( round :: Double -> Integer ) . ( / 6 ) $ -3 + (-1) ^ ( 1 + n' )                    -- associated solution
                                     + 2 ^ ( 3 + n' ) * ( 3 + (-1) ^ ( 1 + 2 * n' ) )
    newLeaf = ( [ clock0Cell { loc = (x,0,2) , isInput = x == sx , label = if x == sx then "SEL" else ""
                             , number = if x == sx then n + 1 else -1 }
                | x <- map ( fromIntegral :: Integer -> Double ) [ sMin n ..  sMin ( n + 1 ) ] ] ++ )
            . ( (++) <*> map ( setX =<< ( sx + 1 + ) . getX ) )
            . map ( \c -> c { isInput = False , label = "" , number = -1 , phase = incrClock 1 $ phase c
                            , loc     = posMath (-) ( loc c ) (0,1,0) } ) . filter ( ( == 2 ) . getZ )
    getNonSelInps = filter ( ( == 0 ) . getZ )
    inps = ( (++) <*> map ( \c -> c { isInput = False , label = "" , number = -1
                                    , loc     = posMath (+) (0,0,1) $ loc c } ) )
         . ( (++) <*> map ( \c -> c { number = number c + 2 ^ ( n + 1 )
                                    , loc    = posMath (+) ( 4 * 2 ^ n , 0 , 0 ) $ loc c } ) )
         . map ( \c -> c { phase = incrClock 1 $ phase c
                         , loc   = posMath (-) ( loc c ) (0,1,0) } ) . getNonSelInps
    wires ins outs = ( \(w,m) ->
      let minY    = minimum . map getY $ w
          propExt = concatMap ( \o -> [ setY newY o { propagate = newY == minY }
                     | newY <- [ minY .. getY ( head outs ) ] ] ) outs
          (w',m') = if length propExt <= 4 then (w,m)
                    else ( map ( \c -> c { phase = incrClock ( if getY c == minY then 1
                                                               else 2 ) $ phase c } ) w , m + 2 )
      in (,m') $ propExt ++ map ( setY minY . setZ 3 ) outs ++ w' )
                   . makeConnectorWires ( map ( ( setZ 4 . ) =<< setX . ( rootOffset + ) . getX )
                   . take ( 2 ^ n ) $ ins ) . map ( setZ 4 ) . take ( 2 ^ n ) $ outs
    root m = map ( \c -> let (x,y,z) = loc c
                             c' = if z /= 0 then c { propagate = False }
                                  else c { isInput = False , label = "" , number = -1 }
                             c'' = if m < 1 then c'
                                   else c' { phase = incrClock m $ phase c' }
                         in c'' { loc = ( x + rootOffset , y , z + 4 ) } ) . filter ( ( /= 0 ) . getZ )


anyAt :: Double -> Double -> [Cell] -> Bool
anyAt x y = any ( ( \(x',y',_) -> x == x' && y == y' ) . loc )

makeConnectorWires :: [Cell] -> [Cell] -> ( [Cell] , Int )
makeConnectorWires outs ins = first addMirroredHalf . ( setClockPhases (ins, outs) =<< minimum . map getY )
                                                      . makeLeftHalf $ zip ins outs
  where
    goUpTill (maxX,maxY) c layer = let (x,y,z) = loc c ; y' = if x == maxX then y + 1 else y
                                   in [ c { loc = (x,newY,z) }
                                      | newY <- [ y' .. findYLimit x y' maxY layer ] ]
    findYLimit x y maxY = minimum . ( maxY : ) . map ( subtract 2 . getY )
                                               . filter ( \c -> getX c == x && getY c > y )
    goRightTill maxX c = [ c { loc = ( newX , getY c , getZ c ) } | newX <- [ getX c + 1 .. maxX ] ]
    fixLeads [] ce _ = ce
    fixLeads (fi:is) ce minY = let newLeads = [ i { loc = ( getX i , minY , getZ i ) }
                                              | i <- fi : is , not $ anyAt ( getX i ) minY ce ]
                                   newIs = ( if anyAt ( getX fi ) ( minY + 1 ) ( newLeads ++ ce ) then id
                                             else ( fi : ) ) is
                               in newLeads ++ fixLeads newIs ce ( minY + 1 )
    findNewY i ce = let (x,y,_) = loc i
                    in if anyAt x y ce then findNewY ( i { loc = posMath (-) ( loc i ) (0,2,0) } ) ce
                       else if not $ anyAt x ( y + 1 ) ce then i
                            else findNewY ( i { loc = posMath (-) ( loc i ) (0,1,0) } ) ce
    makePath (i,o) ce = let (maxX,maxY,_) = loc o
                            i' = findNewY i ce
                            ups1   = goUpTill   (maxX,maxY) i' ce
                            rights = goRightTill maxX ( last $ i' : ups1 )
                            ups2   = if null rights then [] else goUpTill (maxX,maxY) ( last rights ) ce
                        in ups1 ++ rights ++ ups2
    makeLeftHalf = ( ( <*> ( minimum . map getY ) ) . fixLeads . tail . reverse . map fst )
                       <*> foldl ( flip $ (++) <=< makePath ) []
    addMirroredHalf ce = ce ++ let maxX = maximum . map getX $ ce
                               in map ( setX =<< (-) ( 2 * maxX + 2 ) . getX ) ce

setClockPhases :: ([Cell],[Cell]) -> Double -> [Cell] -> ( [Cell] , Int )
setClockPhases (ins,outs) minY ce = let (newCE,maxClockDiff) = foldr ( \i ->
                                          let (x,_,z) = loc i
                                          in uncurry ( ( . max ) . (***) . (++) ) . walk
                                           $ Just i { loc = (x,minY,z) } ) ( [] , 0 ) ins
                                        outClock = incrClock maxClockDiff . phase . head $ ins
                                    in ( syncClocks outClock newCE , maxClockDiff )
  where
    next c = let (x,y,z) = loc c in if anyAt x ( y + 1 ) ce then Just c { loc = ( x , y + 1 , z ) }
                                    else if anyAt ( x + 1 ) y ce then Just c { loc = ( x + 1 , y , z ) }
                                         else Nothing
    nextPhase c = let (x,y,_) = loc c in not ( anyAt x ( y - 2 ) ce ) && anyAt ( x + 1 ) ( y - 2 ) ce
                                      || not ( anyAt x ( y - 1 ) ce   || anyAt ( x - 1 ) ( y - 4 ) ce
                                            || anyAt x ( y - 3 ) ce ) && anyAt x ( y - 4 ) ce
                                      || y == minY && anyAt ( x - 1 ) y ce 
                                         && ( anyAt x ( y + 1 ) ce || anyAt ( x + 1 ) ( y + 1 ) ce )
                                      || y /= minY && not ( anyAt ( x - 1 ) y ce )
                                         && not ( anyAt x ( y + 1 ) ce ) && anyAt x ( y + 2 ) ce
    walk Nothing    = ( [] , 0 )
    walk ( Just c ) = if nextPhase c then let c' = c { phase = incrClock 1 $ phase c }
                                          in bimap ( c' : ) ( + 1 ) . walk . next $ c'
                      else first ( c : ) . walk . next $ c
    replaceCells cs = ( cs ++ ) . filter ( `notElem` cs )
    syncClocks outClock = flip ( foldr ( \o -> let (ox,oy,z) = loc o in replaceCells .
                                   foldr ( \(x,y) cs -> if not ( anyAt x y ce ) || y == minY + 1 then []
                                                        else defaultCell { loc = (x,y,z)
                                                                         , phase = outClock } : cs
                                   ) [] . zip ( repeat ox ) $ reverse [ minY .. oy ] ) ) outs
-- | <<