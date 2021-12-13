module Lib where

import Control.Monad.State
import Data.Char (digitToInt)
import Data.Functor.Identity (Identity)
import Data.List (transpose)
import System.Random (Random (random), RandomGen (genRange), StdGen, mkStdGen)

import Day4 ( day4_1, day4_2 )
import Day5 (day5_1, day5_2)
import Day6 (day6_1)
import Day7 (day7_1)

someFunc :: IO ()
someFunc = do
  -- day1_1
  -- day1_2
  -- day2_1
  -- day2_2
  day4_1
  day4_2 
  day5_1 
  day5_2 

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

-- day3_2
day3_2 :: IO ()
day3_2 = do
  contents <- readFile "input_3.txt"
  print $ runDay3_2 . lines $ contents

runDay3_2 :: [String] -> Int
runDay3_2 xs = oxygenGenRate xs 0 * co2GenRate xs 0

oxygenGenRate :: [String] -> Int -> Int
oxygenGenRate xs i = genRate xs i True

co2GenRate :: [String] -> Int -> Int
co2GenRate xs i = genRate xs i False

genRate :: [String] -> Int -> Bool -> Int
genRate [x] _ _ = binaryToInt x
genRate xs i flag =
  let len = length xs
      ones = foldr (\a b -> b + digitToInt (a !! i)) 0 xs
      a = if flag then '1' else '0'
      b = if flag then '0' else '1'
      mostChar = if ones * 2 >= len then a else b
   in genRate (filter (\a -> a !! i == mostChar) xs) (i + 1) flag

binaryToInt :: String -> Int
binaryToInt = foldl (\b a -> b * 2 + digitToInt a) 0
