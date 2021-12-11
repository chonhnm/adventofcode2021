module Day5 (day5_1) where

import Control.Monad.RWS.Strict (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)
import Text.Read (Lexeme (String))

-- day4_1
day5_1 :: IO ()
day5_1 = do
  contents <- readFile "input_5.txt"
  process contents

process :: String -> IO ()
process xs = do
  case runParse xs of
    Left a -> print a
    Right b -> print $ play b

play l =
  let fl = filter (\x -> horiz x || vertic x || diag x) l
      ps = concatMap lineToPoint fl
      m = pointsToMap ps
   in length $ filter (> 1) $ Map.elems m

--(trace ("fl:" ++show fl ++ "; \nps:" ++show ps) m)

pointsToMap :: [Point] -> Map Point Int
pointsToMap = toMap Map.empty
  where
    toMap m [] = m
    toMap m (x : xs) =
      case Map.lookup x m of
        Nothing -> toMap (Map.insert x 1 m) xs
        Just c -> toMap (Map.update (\x -> Just $ x + 1) x m) xs

horiz :: Line -> Bool
horiz line = (x . a) line == (x . b) line

vertic :: Line -> Bool
vertic line = (y . a) line == (y . b) line

diag :: Line -> Bool
diag (L p1 p2) = diagPoint p1 p2

diagPoint :: Point -> Point -> Bool
diagPoint (P ax ay) (P bx by) =
  let dx = abs $ ax - bx
      dy = abs $ by - ay
   in dx == dy

lineToPoint :: Line -> [Point]
lineToPoint (L p1@(P ax ay) p2@(P bx by)) =
  let xl = if ax > bx then bx else ax
      xh = if ax > bx then ax else bx
      yl = if ay > by then by else ay
      yh = if ay > by then ay else by
   in if xl == xh
        then [P xl y | y <- [yl .. yh]]
        else
          if yl == yh
            then [P x yl | x <- [xl .. xh]]
            else
              let k = (ax - bx) `div` (ay - by)
               in [P x ((x - ax) * k + ay) | x <- [xl .. xh]]

data Point = P
  { x :: Int,
    y :: Int
  }
  deriving (Eq, Ord)

instance Show Point where
  show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

data Line = L
  { a :: Point,
    b :: Point
  }
  deriving (Show)

number :: Parser Int
number = read <$> many1 digit

eol :: Parser ()
eol = void (char '\n') <|> eof

parsePoint :: Parser Point
parsePoint =
  do
    a <- number
    spaces
    char ','
    spaces
    P a <$> number

parseLine :: Parser Line
parseLine =
  do
    a <- parsePoint
    spaces
    string "->"
    spaces
    b <- parsePoint
    eol
    return $ L a b

expr :: Parser [Line]
expr = many1 parseLine

runParse :: String -> Either ParseError [Line]
runParse = parse expr "lines"
