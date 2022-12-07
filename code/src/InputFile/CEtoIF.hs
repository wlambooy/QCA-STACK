{-# LANGUAGE LambdaCase #-}

module InputFile.CEtoIF where


import Data.List
import Data.List.Split
import Data.Bifunctor ( bimap , second)
import Data.Maybe ( isJust , fromJust )

import Cell.Cell
import Cell.Phase
import Data.Char ( intToDigit )


fill :: [Double] -> [Double]
fill = concat . filter ( ( == 1 ) . length ) . group . sort . concat .
    unfoldr ( \case
      []       -> Nothing
      [x]      -> Just ([x],[])
      x1:x2:xs -> if int x1 then if x1 + 0.5 <= x2 then Just ( [x1] , x1 + 1 : x2 : xs )
                                 else Just ( [] , x2 : xs )  -- TODO: error handling
                  else if x2 <= intVal x1 + 1 then Nothing   -- idem
                       else Just ( [x1] , intVal x1 + 1 : x2 : xs ) )
  where
    (intVal,int) = (,) . ( fst . ) <*> ( snd . ) $ ( (,) . fromInteger . round ) <*> ( (==) <*> fromInteger . round )

formatCE :: [Cell] -> ( String , [ ( Char , Cell ) ] )
formatCE ce = ( (,) . map fst <*> map ( second fromJust ) . filter ( isJust . snd )) $ unfoldr ( \ (xyz,defI,spc,sep) ->
                case xyz of
                  (   [] ,   [] ,   [] ) -> Nothing
                  (   [] ,  [_] , z:zs ) -> let zz = if sep then z:zs else zs
                                                nSep = zz == zs
                                            in Just ( ('\n',Nothing) , ( (getXs,getYs,zz) , defI , False , nSep ) )
                  (   [] , _:ys ,   zs ) -> let yy = if sep then getYs else ys
                                            in Just ( ('\n',Nothing) , ( (getXs,yy,zs) , defI , False , False ) )
                  ( x:xs , y:ys ,   zz ) ->
                    if spc then Just ( (' ',Nothing) , (xyz,defI,False,sep) )
                    else if sep then Just ( ('=',Nothing) , ( ( xs , y:ys , zz ) , defI , True , True ) )
                         else if null zz then Nothing
                              else Just $ let z:_ = zz ; nsXYZ = ( xs , y:ys , zz ) in
                                case find ( ( (x,y,z) == ) . loc ) ce of
                                  Nothing -> ( (' ',Nothing) , (nsXYZ,defI,True,False) )
                                  Just c -> if label c /= label defaultCell
                                               || number c /= number defaultCell
                                               || isInput c || isOutput c || propagate c
                                              then ( ( labels !! defI , Just c ) , ( nsXYZ , defI + 1 , True , False ) )
                                              else ( ( showFunc c , Nothing ) , (nsXYZ,defI,True,False) )
              ) ( (getXs,getYs,getZs) , 0 , False , True )
  where
    getXs = fill . map getX . concat . filter ( ( == 1 ) . length ) . group . sortBy sortOnX $ ce
    getYs = reverse . fill . map getY . concat . filter ( ( == 1 ) . length ) . group . sortBy sortOnY $ ce
    getZs = fill . map getZ . concat . filter ( ( == 1 ) . length ) . group . sortBy sortOnZ $ ce
    showFunc c = if phase c 0 == Hold && phase c 1 == Hold then case pol c of { -1 -> '-' ; 1 -> '+' ; _ -> '/' }
                 else intToDigit . ( `mod` 4 ) . subtract 1 . ( 4 - ) . fromEnum $ phase c 0
    labels = [ 'A' .. 'Z' ] ++ [ 'a' .. 'z' ] ++ [ 'À' .. ]


toParsable :: ( String , [ ( Char , Cell ) ] ) -> String
toParsable (ss,cs) = ss ++ foldl ( \ s ( ch , c ) -> s ++ "\n" ++ props ch c) "\n$\n" cs
  where
    props ch c = [ch] ++ ":\n" ++ "- clock = " ++ show ( ( fromEnum ( phase c 0 ) - 3 ) `mod` 4 ) ++ "\n" ++ prop c
    prop c = if label c /= label defaultCell
                    then "- label = \"" ++ label c ++ "\"\n" ++ prop ( c { label = label defaultCell } )
                    else if number c /= number defaultCell
                           then "- number = " ++ show ( number c ) ++ "\n" ++ prop ( c { number = number defaultCell } )
                           else if isInput c then "- input\n" ++ prop ( c { isInput = False } )
                                else if isOutput c then "- output\n" ++ prop ( c { isOutput = False } )
                                     else if propagate c then "- propagate\n" ++ prop ( c { propagate = False } )
                                          else ""


writeCE :: FilePath -> [Cell] -> IO ()
writeCE outFile = writeFile outFile . toParsable . formatCE