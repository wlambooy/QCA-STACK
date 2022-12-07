{-# LANGUAGE LambdaCase #-}

module Cell.IO where

import Cell.Cell
import Cell.Phase

import Data.List ( sortBy , groupBy , minimumBy , maximumBy , sort , find )
import Data.List.Split


prettyPrint :: Output -> IO ()
prettyPrint = foldr ( \( (t,s) , outCells ) xs -> do
    putStrLn $ "Time: " ++ show t
    putStrLn $ "Stable: " ++ show s
    putStrLn $ foldr ( \oc ocs -> show ( loc oc ) ++ ":\t" ++ show ( pol oc ) ++ "\n" ++ ocs ) [] outCells
    xs ) ( pure () )


-- | Thesis V1.1 - Section 3.3.2 >>
makeOutputGroups :: Int -> [Cell] -> [[Cell]]
makeOutputGroups _ [] = []
makeOutputGroups maxNum outs = ( case makeOutputGroup (-1) outs of [] -> makeOutputGroup 0 outs ; xs -> xs )
                               : makeOutputGroups maxNum ( skipTillNext outs )
  where
    skipTillFirst = dropWhile ( ( /= 0 ) . number )
    skipTillNext [] = []
    skipTillNext (x:xs) = if number x == -1 then xs
                          else skipTillFirst . dropWhile ( ( == 0 ) . number ) $ skipTillFirst (x:xs)
    take' n = \case False -> ( : [] ) . head ; True -> takeWhile ( ( == n ) . number )
    makeOutputGroup n out = case dropWhile ( \c -> number c /= n && number c >= 0 ) out of
        [] -> []
        os -> let b = ( >= 0 ) . number $ head os
              in if n == maxNum then take' n b os
                 else case makeOutputGroup ( n + 1 ) os of [] -> [] ; xs -> take' n b os ++ xs

onlyHoldPhase :: Output -> Output
onlyHoldPhase = filter ( not . null . snd ) . map ( \( (t,s) , ocs ) ->
                                                     ( (t,s) , map ( \c -> c { pol = ( pol c + 1 ) / 2 } )
                                                             . filter ( \c -> phase c t == Hold ) $ ocs ) )

parseOutput :: Output -> [[Cell]]
parseOutput = ( makeOutputGroups =<< getMaxOutputNum ) . reverse . concatMap snd . onlyHoldPhase

prettyPrintIO :: [Input] -> Output -> IO ()
prettyPrintIO i = foldr ( \(inCells,outCells) xs -> do
    putStrLn " ============"
    putStrLn $ foldr ( \(ic,ch) ics -> identAndSpacer ic ++ show ch ++ ics )
                  "" ( sortBy ( ( . fst) . compare . fst ) inCells )
    putStrLn $ foldr ( (++) . show ) "\n" ( sort outCells )
    xs ) ( putStrLn " ============" ) . zipIO i . parseOutput
  where zipIO a b = if length b > length a then zipIO a ( tail b ) else zip a b
-- | <<


lineBreak :: (Int,Int) -> String
lineBreak (x,z) = ' ' : concat ( replicate ( z * ( 2 * x + 8 ) - 7 ) "\ESC[1;4;37m " ) ++ "\n\n"

cellEnvToString :: Bool -> Time -> [Cell] -> ( String , (Int,Int) )
cellEnvToString showClocks t ce = let (minX,maxX) = xBounds ce ; (minY,maxY) = yBounds ce
                                      layers = ( addEmptyLayers . groupBy groupOnZ . sortBy sortOnZ ) ce
                                      showFunc = if showClocks then showClock else showPol in (
    ( ++ "\ESC[0m\n") . unlines . reverse . foldr ( (<|>) . lines ) [] . splitOn "\n\n\n" $
    foldr ( \cs -> (++) $ let z = ( getZ . head ) cs in
      foldr ( \(px,py) -> (++) $ if px == -1 then "\n"
        else maybe "\ESC[0m "    showFunc ( find ( ( ( px + minX - 0.5 , py , z ) == ) . loc ) cs )
          ++ maybe "\ESC[1;90m." showFunc ( find ( ( ( px + minX       , py , z ) == ) . loc ) cs )
      ) "\n\n" [ (x,y) | y <- [ minY .. maxY ] , x <- [ 0 .. maxX - minX ] ++ [-1] ]
    ) "" layers , ( floor ( maxX - minX ), length layers ) )
  where
    (<|>) a = \case [] -> a ; b -> zipWith ( ( . ( "\ESC[0m\t\t" ++ ) ) . (++) ) a b
    addEmptyLayers [] = []
    addEmptyLayers ls = let d = foldr ( \(i,cs) n -> if ( ( == i) . getZ . head ) cs then 1 + n
                                                     else 0 ) 0 ( zip [ ( getZ . head . head ) ls .. ] ls )
                            ds = drop d ls
                        in take d ls ++ if null ds then [] else [] : addEmptyLayers ds
    groupOnZ = ( . getZ ) . (==) . getZ
    xBounds = ( (,) . getX . minimumBy sortOnX ) <*> ( getX . maximumBy sortOnX )
    yBounds = ( (,) . getY . minimumBy sortOnY ) <*> ( getY . maximumBy sortOnY )
    showPol c = "\ESC[" ++ ( if isInput c || isOutput c then "4;" else "" ) ++
                let cm
                      | phase c t == Hold = "1;91m"
                      | phase c t == Release = "0;37m"
                      | otherwise = "0;31m"
                    cp
                      | phase c t == Hold = "1;92m"
                      | phase c t == Release = "0;37m"
                      | otherwise = "0;32m"
                    co = if phase c t == Hold then "1;37m" else "0;37m"
                in case signum ( pol c ) of -1 -> cm ++ "-" ; 0 -> co ++ "o" ; 1 -> cp ++ "+" ; _ -> ""
    showClock c = if phase c t == phase c ( t + 1 ) then showPol c 
                  else show . ( `mod` 4 ) . ( 3 - ) . fromEnum $ phase c t

showCellEnv :: Time -> [Cell] -> IO ()
showCellEnv t = putStrLn . fst . cellEnvToString False t

showCEClocks :: [Cell] -> IO ()
showCEClocks = putStrLn . fst . cellEnvToString True 0