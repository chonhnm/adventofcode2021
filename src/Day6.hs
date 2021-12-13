module Day6 where

day6_1 :: IO ()
day6_1 = do
  contents <- readFile "input_6.txt"
  let input = parseInput contents
  print $ partOne input
  print $ partTwo input

oneStep :: Num a => [a] -> [a]
oneStep [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]
oneStep _ = []

parseInput :: [Char] -> [Int]
parseInput input = map (`count` input) "012345678" 

count :: Char -> [Char] -> Int
count = (length .) . filter . (==)

partOne :: Num a => [a] -> a
partOne input = sum $ iterate oneStep input !! 80

partTwo :: Num a => [a] -> a
partTwo input = sum $ iterate oneStep input !! 256