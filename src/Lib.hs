module Lib
  ( someFunc,
  )
where

import Control.Monad.State
import Data.Functor.Identity (Identity)
import Data.List (transpose)
import System.Random (Random (random), StdGen, mkStdGen)
import Data.Char (digitToInt)

someFunc :: IO ()
someFunc = do
  day1_1
  day1_2
  day2_1
  day2_2
  day3_1

g :: StdGen
g = mkStdGen 666

test :: Int
test = let (a, g') = random g in a `mod` 100

t1 :: StateT Integer Identity (Integer, Integer)
t1 = do
  a <- get
  modify (+ 1)
  b <- get
  return (a, b)

run_t1 :: ((Integer, Integer), Integer)
run_t1 = runState t1 0

day1_1 :: IO ()
day1_1 = do
  contents <- readFile "input_1.txt"
  print . largeThanPreviousCount . map readInt . words $ contents

day1_2 :: IO ()
day1_2 = do
  contents <- readFile "input_1.txt"
  print . largeThanPreviousCount . thrrunIdentityTSumUp . map readInt . words $ contents

readInt :: String -> Int
readInt = read

largeThanPreviousCount :: [Int] -> Int
largeThanPreviousCount xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

-- sum up thrrunIdentityT continuous element

thrrunIdentityTSumUp :: [Int] -> [Int]
thrrunIdentityTSumUp xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)

-- day2
day2_1 :: IO ()
day2_1 = do
  contents <- readFile "input_2.txt"
  print . uncurry (*) . position . lines $ contents

position :: [String] -> (Int, Int)
position = foldr (listToTuple . pos) (0, 0)

listToTuple :: (Int, Int) -> (Int, Int) -> (Int, Int)
listToTuple (a, b) (c, d) = (a + c, b + d)

pos :: String -> (Int, Int)
pos x =
  let [cmd, valStr] = words x
      val = readInt valStr
   in case cmd of
        "forward" -> (val, 0)
        "up" -> (0, - val)
        "down" -> (0, val)
        _ -> (0, 0)

-- day2_2
day2_2 :: IO ()
day2_2 = do
  contents <- readFile "input_2.txt"
  print $ run . lines $ contents

data Position = Position {horizon :: Int, depth :: Int, aim :: Int}

evalInput :: [String] -> State Position Int
evalInput [] = do
  pos <- get
  return $ horizon pos * depth pos
evalInput (x : xs) = do
  pos <- get
  put $ getNewPos pos x
  evalInput xs

getNewPos :: Position -> String -> Position
getNewPos (Position h d a) x =
  let [cmd, valStr] = words x
      val = readInt valStr
   in case cmd of
        "forward" -> Position (h + val) (d + val * a) a
        "up" -> Position h d (a - val)
        "down" -> Position h d (a + val)
        _ -> error $ "not support command: " ++ cmd

startState :: Position
startState = Position 0 0 0

run :: [String] -> Int
run xs = evalState (evalInput xs) startState

-- day3
day3_1 :: IO ()
day3_1 = do
  contents <- readFile "input_3.txt"
  print $ runDay3_1 . lines $ contents

runDay3_1 :: [String] -> Int
runDay3_1 xs = let a = strToOnes xs in gamma a * epsilon a

strToOnes :: [String] -> [(Int, Int)]
strToOnes xs = map strToOne $ transpose xs

strToOne :: String -> (Int, Int)
strToOne xs = (foldr (\d a -> digitToInt d + a) 0 xs, length xs)

gamma' :: [(Int, Int)] -> [Int]
gamma' = map (\(a, b) -> if a >= b `div` 2 then 1 else 0)

epsilon' :: [(Int, Int)] -> [Int]
epsilon' = map (\(a, b) -> if a < b `div` 2 then 1 else 0)

sumUp :: [Int] -> Int
sumUp = foldl (\r e -> r*10 + e) 0

gamma :: [(Int, Int)] -> Int
gamma = sumUp . gamma'

epsilon :: [(Int, Int)] -> Int
epsilon = sumUp . epsilon'