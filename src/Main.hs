module Main where
import Control.Applicative ((<$>), (<*>), (*>), (<*), many, pure, (<|>))
import Data.Attoparsec.Text (Parser, digit, takeWhile, space, char, many1, inClass, many', sepBy', parseOnly)
import Data.Text (Text, justifyRight, pack)
import Data.Monoid ((<>))
import Data.Char (digitToInt)
import Data.Text.IO (getLine)
import Prelude hiding (getLine, takeWhile)

type Predicate = Text
type End = Text
type Len = Int

data Host = Host Predicate Len Int End 
          | Range Host Host
          | Enum [Host] 
          deriving (Show, Eq)

type SHost = Text

eval :: Host -> [SHost]
eval (Host p l i e) = [p <> justifyRight l '0' (tT i) <> e]
eval (Range (Host p l i e) (Host _ _ i1 _)) = concatMap (\x -> eval (Host p l x e)) [i .. i1]
eval (Enum x) = concatMap eval x

tT :: Int -> Text
tT = pack . show


host :: Parser Host
host = do
    p <- preds
    d <- many digit
    e <- preds
    return $ Host p (length d) (digitsToInt d) e
    where
      digitsToInt :: String -> Int
      digitsToInt i = sum . map (uncurry (*)) $ zip razr (rv i)
      rv = reverse . map digitToInt
      razr = [1,10,100,1000,10000,100000,1000000,10000000]
      preds = takeWhile (inClass $ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ "-")

range :: Parser Host
range = Range <$> (host <* many1 space ) <*> host 

separator :: Parser ()
separator = many' space *> char ',' *> many' space *> pure ()

enum :: Parser Host
enum = Enum <$> (range <|> host ) `sepBy'` separator

hosts :: Parser Host
hosts = char '[' *> enum <* char ']'

main :: IO ()
main = do
    s <- getLine
    case eval <$> parseOnly hosts s of
         Right x -> print x
         Left _ -> print "bad hosts range"
    
