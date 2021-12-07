module Day4 where

import Text.Parsec
import Data.Char
import Text.Parsec.String (Parser)
import Control.Monad.RWS.Strict

lineOfInput :: Parser [Int]
lineOfInput = sepBy number (char ',')

number :: Parser Int
number = do
    s <- string "-" <|> return []
    cs <- many1 digit
    return $ read $ s ++ cs

lineOfGrid :: Parser [Int]
lineOfGrid = do
    many (char ' ')
    r <-sepBy1 number (many1 $ char ' ')
    eol  
    return r 

eol :: Parser ()
eol = void (char '\n') <|> eof  

grid :: Parser [[Int]]
grid = many lineOfGrid

grids :: Parser [[[Int]]]
grids = sepBy grid (char  '\n')

parseAll :: Parser Bingo
parseAll = do
    i <- lineOfInput
    many1 space
    Bingo i <$> grids

type Grid = [[Int ]]

data Bingo = Bingo {
  inp :: [Int],
  grds :: [Grid]
}  deriving (Show)


runParse :: String -> Either ParseError Bingo
runParse  = parse parseAll "bingo"

process :: String -> IO ()
process xs = do
    let res = runParse xs
    case res of
        Left err -> print err
        Right b -> print $ playBingo b

playBingo :: Bingo -> Int
playBingo = error "not implemented"

