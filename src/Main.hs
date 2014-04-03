{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Applicative ((<$>), (<*>), (*>), (<*), many, pure, (<|>))
import Data.Attoparsec.Text (Parser, digit, takeWhile, space, char, many1, inClass, many', sepBy', parseOnly)
import Data.Text (Text, justifyRight, pack, take, drop, init, length)
import Data.Monoid ((<>))
import Data.Char (digitToInt)
import Data.Text.IO (getLine)
import Data.Conduit (await, yield, (=$), ($$), ($=), Conduit)
import Data.Conduit.Text (decodeUtf8, encodeUtf8, linesBounded)
import Data.Conduit.Network (runTCPServer, appSource, appSink, Application, HostPreference( HostAny ), serverSettings)
import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import qualified Prelude as P
import Prelude hiding (getLine, takeWhile, take, drop, init, length)

main :: IO ()
main = doWork =<< getArgs 
    where
      doWork [] = do
          s <- getLine
          case work s of
               Right x -> (print x) >> exitSuccess
               Left a -> (print $ "bad hosts range " <> a) >> exitFailure
      doWork x = case head x of
                     "-s" -> server
                     _ -> (print "bad usage") >> exitFailure

data Host = Host Text  
          | Range Text [(Int, Int, Int)] Text
          | Enum [Host] 
          deriving (Show, Eq)

eval :: Host -> [Text]
eval (Host t) = [t]
eval (Range p r e) = concatMap (\(l, i, i') -> concatMap (\x -> [p <> justifyRight l '0' (tT x) <> e]) [ i .. i' ]) r
eval (Enum x) = concatMap eval x

tT :: Int -> Text
tT = pack . show

host :: Parser Host
host = Host <$> hostS 

range :: Parser Host
range = do
    p <- hostS
    char '[' 
    r <- pair `sepBy'` separator
    char ']'
    e <- hostS
    return $ Range p r e

hostS :: Parser Text
hostS = takeWhile (inClass $ ['A' .. 'Z'] ++ ['a' .. 'z']  ++ ['0' .. '9'] ++ ".-") 

pair :: Parser (Int, Int, Int)
pair = do
    ds <- many space *> many digit <* many space
    char '-'
    dl <- many space *> many digit <* many space
    return (P.length ds, digitsToInt ds, digitsToInt dl)
    where
      digitsToInt i = sum . map (uncurry (*)) $ zip razr (rv i)
      rv = reverse . map digitToInt
      razr = [1,10,100,1000,10000,100000,1000000,10000000]

separator :: Parser ()
separator = many' space *> char ',' *> many' space *> pure ()

enum :: Parser Host
enum = Enum <$> (range <|> host ) `sepBy'` separator

hosts :: Parser Host
hosts = many' space *> enum <* many' space 

work :: Text -> Either String [Text]
work s = eval <$> parseOnly hosts s

server :: IO ()
server = runTCPServer (serverSettings 4000 HostAny) hostRangeApp

hostRangeApp :: Application IO 
hostRangeApp appData = do
    let source = appSource appData
        sink = appSink appData
    source $= decodeUtf8 $= linesBounded 67 $$ conduit =$ encodeUtf8 =$ sink

conduit :: Conduit Text IO Text
conduit = do
    l <- await
    case take 4 <$> l of
         Just "get " -> runHost (init . drop 4 <$> l)
         Just "GET " -> runHost (init . drop 4 <$> l)
         _ -> yield "CLIENT_ERROR only get request\r\n"
    where
      lTt x = "[" <> foldl1 (\z y -> z <> "," <> y) x <> "]"
      sizeD = pack . show . length . lTt
      runHost Nothing = yield "CLIENT_ERROR \r\n"
      runHost (Just str) = case work str of
                         Left _ -> do 
                             yield $ "CLIENT_ERROR unused format\r\n"
                         Right x -> do
                             yield $ "VALUE " <> str <> " 0 " <> sizeD x <> " \r\n"
                             yield $ lTt x <> "\r\n"
                             yield "END\r\n"
