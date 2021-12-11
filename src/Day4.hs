module Day4 (day4_1, day4_2) where

import Control.Monad.RWS.Strict
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

-- day4_1
day4_1 :: IO ()
day4_1 = do
  contents <- readFile "input_4.txt"
  process cal1 contents

day4_2 :: IO ()
day4_2 = do
  contents <- readFile "input_4.txt"
  process cal2 contents

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

process :: Cal -> String -> IO ()
process f xs = do
  let res = runParse xs
  case res of
    Left err -> print err
    Right b -> print $ playBingo f b

col, row, size :: Index
col = 5
row = 5
size = col * row

playBingo :: Cal -> Bingo -> Int
playBingo cal bingo =
  let m = gridToMap $ grds bingo
   in cal bingo m

type Cal = Bingo -> Map Value [Index] -> Int

type Cal' = [Value] -> [Index] -> Map Value [Index] -> [Grid] -> Int

cal1 :: Cal
cal1 b m = cal1' (inp b) [] m (grds b)

cal1' :: Cal'
cal1' [] _ _ _ = error "not found winner"
cal1' (x : xs) marked m gs =
  case Map.lookup x m of
    Nothing -> cal1' xs marked m gs
    Just needToMark ->
      let allMarked = marked ++ needToMark
       in case complete x needToMark allMarked of
            Nothing -> cal1' xs allMarked m gs
            Just c -> calValue c gs

cal2 :: Cal
cal2 b m = cal2' (inp b) [] m (grds b) []

cal2' :: [Value] -> [Index] -> Map Value [Index] -> [Grid] -> [Index] -> Int
cal2' [] _ _ _ _ = error "not found winner"
cal2' (x : xs) marked m gs gidxs =
  case Map.lookup x m of
    Nothing -> cal2' xs marked m gs gidxs
    Just needToMark ->
      let allMarked = marked ++ needToMark
          len = length gs
       in case addGrdIdx len x needToMark allMarked gidxs of
            Left a -> cal2' xs allMarked m gs a
            Right b -> calValue2 x b allMarked gs

calValue2 :: Value -> [Index] -> [Index] -> [Grid] -> Int
calValue2 val b marked g =
  let len = fromIntegral $ length g
      cg = CompleteGrid val (head b) marked
   in calValue cg g

addGrdIdx :: Int -> Value -> [Index] -> [Index] -> [Index] -> Either [Index] [Index]
addGrdIdx _ _ [] _ gidx = Left gidx
addGrdIdx size val (x : xs) marked gidx =
  let i = itemIdxToGridIdx x
   in if i `elem` gidx
        then addGrdIdx size val xs marked gidx
        else case complete' val x marked of
          Nothing -> addGrdIdx size val xs marked gidx
          _ ->
            if length gidx == (size - 1)
              then Right (i : gidx)
              else addGrdIdx size val xs marked (i : gidx)

data CompleteGrid = CompleteGrid
  { cg_val :: Value,
    cg_grdIndex :: Index,
    cg_markedIdx :: [Index]
  }
  deriving (Show)

complete :: Value -> [Index] -> [Index] -> Maybe CompleteGrid
complete _ [] _ = Nothing
complete val (x : xs) marked =
  case complete' val x marked of
    Nothing -> complete val xs marked
    a -> a

complete' :: Value -> Index -> [Index] -> Maybe CompleteGrid
complete' val x marked =
  let rem = x `mod` size
      low = x - rem
      high = low + size
      mg = filter (\x -> (x > low) && (x <= high)) marked
      mg' = map (\x -> x - low) mg
      rs = length $ filter (\x -> (x -1) `div` col == rem `div` col) mg'
      cs =
        length $
          filter
            (\x -> x `mod` col == rem `mod` col)
            mg'
   in if rs == fromIntegral col || cs == fromIntegral col
        then Just $ CompleteGrid val (itemIdxToGridIdx x) mg
        else Nothing

itemIdxToGridIdx :: Index -> Index
itemIdxToGridIdx x = fromIntegral x `div` fromIntegral size

calValue :: CompleteGrid -> [Grid] -> Value
calValue c gs =
  let gidx = cg_markedIdx c
      inpVal = cg_val c
      i = fromIntegral $ cg_grdIndex c
      gs' = gs !! i
      fgs = concat $ concat gs
      mVal = getVal gidx fgs
      gs'' = filter (`notElem` mVal) (concat gs')
   in inpVal * sum gs''
  where
    getVal [] fgs = []
    getVal (x : xs) fgs = (fgs !! (fromIntegral x -1)) : getVal xs fgs

gridToMap :: [Grid] -> Map Value [Index]
gridToMap grids = toMap Map.empty $ zip (concat $ concat grids) [1 ..]
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