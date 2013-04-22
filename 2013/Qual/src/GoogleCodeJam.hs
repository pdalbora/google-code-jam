module GoogleCodeJam (
    solver,
    toEOL,
    int,
    integer,
    spaceSep
) where

import Control.Applicative hiding ((<|>))
import Text.Parsec
import Text.Parsec.String (Parser)

solver :: (Show b) => Parser a -> (a -> b) -> (String -> String) 
solver p f = unlines . printSolutions . solveCases . parseCases
        where printSolutions = zipWith printSolution [1..]
              solveCases     = map f
              parseCases s   = either (error . show) id (parse (cases p) "" s)

printSolution :: (Show a) => Integer -> a -> String
printSolution i s = "Case #" ++ show i ++ ": " ++ show s

cases :: Parser a -> Parser [a]
cases p = do { c <- fromInteger <$> toEOL integer
             ; count c p }

toEOL :: Parser a -> Parser a
toEOL = (<* newline)

integer :: Parser Integer
integer = rd <$> number
        where rd = read :: String -> Integer

int :: Parser Int
int = rd <$> number
    where rd = read :: String -> Int

spaceSep :: Parser a -> Parser [a]
spaceSep = (`sepBy` (char ' '))

number :: Parser String
number = many1 digit
