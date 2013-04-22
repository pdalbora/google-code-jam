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
solveCase l = Answer $ all checkRow rows
    where rows = l
          cols = transpose rows
          checkRow r        = allSame r || all isMaxInCol (minima r)
          isMaxInCol (ci,h) = h == maximum (cols !! ci)

minima :: (Ord a) => [a] -> [(Int,a)]
minima xs = filter ((== min) . snd) $ zip [0..] xs
    where min = minimum xs

allSame :: (Eq a) => [a] -> Bool
allSame (x:xs) = all (== x) xs

