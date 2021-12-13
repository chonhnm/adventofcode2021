module Day7 where
import Data.IntMap.Lazy (foldr')
import Debug.Trace ( trace )

day7_1 :: IO ()
day7_1 = do
  contents <- readFile "input_7.txt"
  let input = stringToInt contents
  print $ partOne input
  print $ partTwo input

partOne :: [Int] -> Int
partOne xs = minimum $ map (`diff1` xs) xs

partTwo :: [Int] -> Int
partTwo xs = 
    let s = sum xs
        len = length xs
        avg = s `div` len 
     in min (diff2 avg xs)  (diff2 (avg + 1) xs)

diff2 :: Int -> [Int] -> Int
diff2 x xs = sum $ map (\y -> let a =  abs (x-y) in a*(a+1) `div` 2) xs

diff1 :: Int -> [Int] -> Int
diff1 x xs = sum $ map (\y -> abs (x-y)) xs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

stringToInt :: String -> [Int]
stringToInt xs=  map read (wordsWhen (== ',') xs)   