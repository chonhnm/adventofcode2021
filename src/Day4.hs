module Day4 where

import Control.Monad.RWS.Strict
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

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
  r <- sepBy1 number (many1 $ char ' ')
  eol
  return r

eol :: Parser ()
eol = void (char '\n') <|> eof

grid :: Parser [[Int]]
grid = many lineOfGrid

grids :: Parser [[[Int]]]
grids = sepBy grid (char '\n')

parseAll :: Parser Bingo
parseAll = do
  i <- lineOfInput
  many1 space
  Bingo i <$> grids

type Grid = [[Int]]

type Index = Integer

type Value = Int

data Bingo = Bingo
  { inp :: [Int],
    grds :: [Grid]
  }
  deriving (Show)

runParse :: String -> Either ParseError Bingo
runParse = parse parseAll "bingo"

process :: String -> IO ()
process xs = do
  let res = runParse xs
  case res of
    Left err -> print err
    Right b -> print $ playBingo b

col, row, size :: Index
col = 5
row = 5
size = col * row

playBingo :: Bingo -> Int
playBingo bingo =
  let m = myMap bingo
      xs = inp $ trace ("map:" ++ show m) bingo
   in cal xs [] m (grds bingo)

cal :: [Value] -> [Index] -> Map Value [Index] -> [Grid] -> Int
cal [] _ _ _ = error "not found winner"
cal (x : xs) marked m gs =
  case Map.lookup x m of
    Nothing -> cal xs marked m gs
    Just needToMark ->
      let kkk = marked ++ needToMark
       in case win needToMark kkk gs of
            Nothing -> cal xs kkk m gs
            Just a -> a

win :: [Index] -> [Index] -> [Grid] -> Maybe Int
win [] _ _ = Nothing
win all@(x : xs) marked gs =
  let rem = x `mod` size
      low = x - rem
      high = low + size
      mg = filter (\x -> (x > low) && (x <= high)) marked
      mg' = map (\x -> x - low) mg
    --   r = rem `div` col + 1
    --   c = rem `mod` col
      rs = length $ filter (\x -> (x-1) `div` col == rem `div` col) mg'
      cs =
        length $
          filter
            (\x -> (x) `mod` col == rem `mod` col)
            ( trace
                ( "mg:" ++ show mg ++ "mg':" ++ show mg'
                    ++ "x:"
                    ++ show x
                    ++ " ;rem:"
                    ++ show rem
                    ++ " ;mkd:"
                    ++ show (concat (concat gs) !! (fromIntegral x - 1))
                    ++ " ;marked:"
                    ++ show marked
                    ++ " ;rs:"
                    ++ show rs
                )
                mg'
            )
   in if rs == fromIntegral col || cs == fromIntegral col
        then Just (cal' x mg gs)
        else win xs marked gs
  where
    cal' x mg gs =
      let i = fromIntegral x `div` fromIntegral size
          gs' = gs !! i
          fgs = (concat $ concat gs)
          inpVal = fgs !! (fromIntegral x - 1)
          mVal = getVal mg fgs
          gs'' = filter (`notElem` mVal) (concat gs')
       in trace (show mVal ++ ";gs" ++ show gs'') (inpVal * sum gs'')
      where
        getVal [] fgs = []
        getVal (x : xs) fgs = (fgs !! (fromIntegral x -1)) : getVal xs fgs

myMap :: Bingo -> Map Value [Index]
myMap bingo = toMap Map.empty $ zip (concat $ concat $ grds bingo) [1 ..]
  where
    toMap m [] = m
    toMap m ((x, i) : xs) =
      case Map.lookup x m of
        Nothing ->
          let mm = Map.insert x [i] m
           in toMap mm xs
        Just a ->
          let mm = Map.update (\a -> Just $ a ++ [i]) x m
           in toMap mm xs