module Day6 where

day6_1 :: IO ()
day6_1 = do
  contents <- readFile "input_6_test.txt"
  process $ map read (wordsWhen (== ',') contents)

process :: [Int] -> IO ()
process xs = print $ length $ lifeCycle 80 (map Old xs)

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