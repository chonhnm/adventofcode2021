module Day6 where

day6_1 :: IO ()
day6_1 = do
  contents <- readFile "input_6_test.txt"
  process $ map read (wordsWhen (== ',') contents)

process :: [Int] -> IO ()
process xs = print $ sum $ map (oneLifeCycle 80) xs

process1 :: [Int] -> IO ()
process1 xs = print $ length $ lifeCycle2 80 xs

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

data LanternFish = New Int | Old Int deriving (Show)

lifeCycle :: Int -> [LanternFish] -> [LanternFish]
lifeCycle 0 xs = xs
lifeCycle n xs = lifeCycle (n -1) (reborn xs)
  where
    reborn [] = []
    reborn (Old 0 : xs) = (Old 6 : reborn xs) ++ [New 8]
    reborn (Old n : xs) = Old (n -1) : reborn xs
    reborn (New 0 : xs) = (Old 6 : reborn xs) ++ [New 8]
    reborn (New n : xs) = Old (n -1) : reborn xs

lifeCycle1 :: Int -> [Int] -> [Int]
lifeCycle1 0 xs = xs
lifeCycle1 n xs = lifeCycle1 (n -1) (reborn xs)
  where
    reborn [] = []
    reborn (0 : xs) = (6 : reborn xs) ++ [8]
    reborn (n : xs) = (n -1) : reborn xs

lifeCycle2 :: Int -> [Int] -> [Int]
lifeCycle2 0 xs = xs
lifeCycle2 n xs = lifeCycle1 (n -1) (reborn xs)
  where
    reborn = concatMap (\x -> if x == 0 then [6, 8] else [x -1])

oneLifeCycle :: Int -> Int -> Int
oneLifeCycle n i = 1 + oneLife (n - i - 1) 
 where 
     --oneLife x  = if x < 9 then (x+6) `div` 7 else (x+6) `div` 7 + oneLife (x - 9)
    oneLife x = if x > 0 then 1 + x `div` 7 + oneLife  (x - 8) else 0
