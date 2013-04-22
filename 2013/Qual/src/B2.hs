import GoogleCodeJam
import Data.List
import Data.Ord
import Text.Parsec 
import Text.Parsec.String (Parser)
import Control.Applicative hiding ((<|>))

main :: IO ()
main = interact (solver lawn solveCase)

newtype Answer = Answer Bool
instance Show Answer where
    show (Answer True)  = "YES"
    show (Answer False) = "NO"

newtype Height = Height Integer deriving (Show,Eq,Ord)
type Row = [Height]
type Lawn = [Row]

lawn :: Parser Lawn
lawn = do { (n:_) <- toEOL $ spaceSep integer
          ; count (fromInteger n) row }

row :: Parser Row
row = (map Height) <$> (toEOL $ spaceSep integer)

solveCase :: Lawn -> Answer
solveCase l = Answer $ all checkRow (zip [0..] rows)
    where rows = l
          cols = transpose rows
          checkRow (i,r)      = all (checkSquare i) (zip [0..] r)
          checkSquare i (j,h) = 
              h == maximum (cols !! j) || h == maximum (rows !! i)  
