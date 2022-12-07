module InputFile.IFtoCE where

import Cell.Cell
import Cell.Phase
import DesignTools

import Text.ParserCombinators.Parsec hiding ( space , spaces , label )
import Text.Parsec.Char hiding ( space , spaces )
import Data.Char ( digitToInt , intToDigit , isDigit )
import Control.Monad.Cont ( void )
import Data.Maybe ( fromJust )
import Data.Foldable ( find )
import Data.Functor ( ($>) )
import Data.List.Split hiding ( oneOf )


-- | Thesis V1.1 - Section 3.3.3


space :: Parser ()
space = void $ char ' '

spaces :: Parser ()
spaces = void $ many space

eol :: Parser ()
eol = void $ spaces >> endOfLine


comment :: Parser ()
comment = void $ char '[' >> manyTill anyChar ( char ']' ) >> many1 eol


layerSep :: Parser Int
layerSep = char '=' `sepEndBy1` space >>= ( eol $> ) . length

layerSepN :: Int -> Parser ()
layerSepN n = void $ char '=' >> count ( n - 1 ) ( space >> char '=' ) >> eol


cell :: Parser Char
cell = oneOf " +-" <|> letter <|> intToDigit . flip mod 4 . digitToInt <$> digit

cellRow :: Int -> Parser String
cellRow n = sepEndBy cell space >>= \cs -> eol >> let l = length cs
                                                  in if l > n then if any ( /= ' ' ) $ drop n cs
                                                                     then fail "cell row too long"
                                                                     else pure $ take n cs
                                                     else pure $ cs ++ replicate ( n - l ) ' '

layer :: Int -> Parser [String]
layer n = manyTill ( cellRow n ) ( layerSepN n )

layerN :: Int -> Int -> Parser [String]
layerN c n = n `count` cellRow c >>= ( layerSepN c $> )

layers :: Parser [ [String] ]
layers = layerSep >>= \c -> layer c >>= fmap . (:) <*> ( many . try . layerN c . length )


assign :: Parser ()
assign = void $ many space >> char '=' >> many space

stringToPos :: String -> Pos
stringToPos s = let [x,y,z] = map ( read :: String -> Double ) $ splitOn "," s in (x,y,z)

definition :: Parser Cell
definition = string "- " >>
      ( ( string "input"                      $> defaultCell { isInput   = True } )
    <|> try ( string "output"                 $> defaultCell { isOutput  = True } )
    <|> ( string "propagate"                  $> defaultCell { propagate = True } )
    <|> ( string "number" >> assign >> ( \num -> defaultCell { number = num } ) . read <$> many1 digit )
    <|> ( string "label"  >> assign >> ( \lbl -> defaultCell { label  = lbl } )
      <$> between ( char '"' ) ( char '"' ) ( many $ noneOf "\"" ) )
    <|> ( string "clock"  >> assign >> ( \dgt -> defaultCell { phase  = clock dgt } ) . flip mod 4 . digitToInt
      <$> digit )
    <|> ( string "offset" >> assign >> ( \pos -> defaultCell { loc = pos } ) . stringToPos
      <$> between ( char '(' ) ( char ')' ) ( many $ char '-' <|> digit <|> char '.' <|> char ',' ) ) )


build :: [Cell] -> Cell
build = foldr merge . head <*> tail
  where merge c1 c2 = defaultCell { isInput   = isInput   c1 || isInput   c2
                                  , isOutput  = isOutput  c1 || isOutput  c2
                                  , propagate = propagate c1 || propagate c2
                                  , label     = label     c1 ++ label     c2
                                  , loc    = loc $ if loc c1       == loc defaultCell     then c2 else c1
                                  , phase  = phase $ if phase c1 0 == phase defaultCell 0 then c2 else c1
                                  , number = number $ if number c1 == number defaultCell  then c2 else c1 }

makeCell :: Parser Cell
makeCell = build <$> many1 ( definition >>= ( ( eol <|> eof ) $> ) )


cellDefinition :: Parser (Char,Cell)
cellDefinition = letter >>= ( <$> ( char ':' >> eol >> makeCell ) ) . (,)


beforeSep :: Parser [ [String] ]
beforeSep = many eol >> ( ( comment >> layers ) <|> layers )

separator :: Parser ()
separator = void $ many eol >> char '$' >> many1 eol


parseInput :: Parser ( [ [String] ] , [(Char,Cell)] )
parseInput = beforeSep >>= ( <$> ( separator >> cellDefinition `sepEndBy` many eol ) ) . (,)


charToCell :: [(Char,Cell)] -> Pos -> Char -> Cell
charToCell defs pos ch = let c = case ch of
                              '-' -> negativeCell ; '+' -> positiveCell ;
                              x   -> if isDigit x then defaultCell { phase = clock $ digitToInt x }
                                     else ( snd . fromJust . find ( ( == x ) . fst ) ) defs
                         in c { loc = posMath (+) pos $ loc c }

makeCellEnv :: ( [ [String] ] , [(Char,Cell)] ) -> [Cell]
makeCellEnv (cellMap,defs) = concat . concat $ zipWith ( \z -> zipWith ( \y s ->
    [ charToCell defs (x,-y,z) c | (x,c) <- zip [ 0 .. ] s , c /= ' ' ] ) [ 0 .. ] ) [ 0 .. ] cellMap


parseFile :: FilePath -> IO ( Either ParseError [Cell] )
parseFile fp = fmap makeCellEnv <$> parseFromFile parseInput fp