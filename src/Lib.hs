module Lib
  ( someFunc,
  )
where

import Control.Monad.State
import Data.Functor.Identity (Identity)
import System.Random (Random (random), StdGen, mkStdGen)
import Data.List (transpose)



someFunc :: IO ()
someFunc = do
  day1_1
  day1_2
  day2_1

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
  print . largeThanPreviousCount . threeSumUp . map readInt . words $ contents

readInt :: String -> Int
readInt = read

largeThanPreviousCount :: [Int] -> Int
largeThanPreviousCount xs = length $ filter (uncurry (<)) $ zip xs (tail xs)

-- sum up three continuous element

threeSumUp :: [Int] -> [Int]
threeSumUp xs = zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)

-- day2
day2_1 :: IO ()
day2_1 = do
  contents <- readFile "input_2.txt"
  print . uncurry (*) . listToTuple . position . lines $ contents

listToTuple :: [Int] -> (Int, Int)
listToTuple [a, b] = (a, b)

position :: [String] -> [Int]
position xs = map sum $ transpose  (map pos xs)

pos :: String -> [Int]
pos x =
  let [cmd, valStr] = words x
      val = readInt valStr
   in case cmd of
        "forward" -> [val, 0]
        "up" -> [0, - val]
        "down" -> [0, val]
        _ -> [0, 0]