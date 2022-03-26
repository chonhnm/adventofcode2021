module Bird03 where

import Data.List

-- 3.1 List notation

-- 3.1.1
numlist :: [[Integer]]
numlist = [[1, 2, 3], [], [4, 5, 6]]

charlist :: [[Char]]
charlist = ["how", [], "are", "you"]

-- 3.1.2
-- length [a..b] = b - a + 1
-- length [a,b..c] = ((c - a) div (b - a)) + 1

-- 3.2 List comprehensions

divisors :: Integral a => a -> [a]
divisors n = [d | d <- [1 .. n], n `mod` d == 0]

ugcd :: Integral a => a -> a -> a
ugcd a b = maximum [d | d <- divisors a, b `mod` d == 0]

prime :: Integral a => a -> Bool
prime n = divisors n == [1, n]

-- 3.2.1
k :: [Integer]
k = [j | i <- [1, -1, 2, -2], i > 0, j <- [1 .. i]]

-- 3.2.3
cntneg :: (Ord a, Num a) => [a] -> Int
cntneg xs = length [1 | x <- xs, x < 0]

-- 3.2.4
intpairs :: (Num b, Enum b) => b -> [(b, b)]
intpairs n = [(i, j) | i <- [1 .. n], j <- [1 .. n]]

-- 3.2.5
quadruples :: (Num d, Eq d, Enum d) => d -> [(d, d, d, d)]
quadruples n =
  [ (a, b, c, d)
    | a <- [1 .. n],
      b <- [a .. n],
      c <- [1 .. n],
      d <- [c .. n],
      a ^ 2 + b ^ 2 == c ^ 2 + d ^ 2
  ]

-- 3.2.6
expt :: (Num a, Num t, Enum t) => a -> t -> a
expt x n = product [x | a <- [1 .. n]]

-- 3.2.7 divisors 0 = []

-- 3.3 Operations on lists
{-
++
concat
length
head and tail
init and last
take and drop : take n xs ++ drop n xs == xs, take 0 xs = [], drop 0 xs = xs
takeWhile and dropWhile
reverse
zip
zipWith
(!!): position. [1,2,3] !! 1 == 2
(\\): list difference
-}
jj :: [Integer]
jj = [1, 2, 3, 3, 1] \\ [1, 3]

-- 3.4 Map and filter

jjj :: [a -> b] -> [[a] -> [b]]
jjj = map map

-- 3.5 The fold operators
-- Laws
-- First duality theorem:
{-
           foldr (+) a xs == foldl (+) a xs
 whenever + and a form a monoid and xs is a finite list.
 It is better to define `sum` and `product` using foldl, and better to define
 `concat`, `and`, and `or` using foldr
-}
--Second duality theorem:
{-
Suppose + and * and a are such that for all x, y and z we have:
           x + (y * z) = (x + y) * z
           x + a = a * z
we have:
           foldr (+) a xs = foldl (*) a xs
-}
-- Thrid duality theorem:
{-
           foldr (+) a xs = foldl (~+) a (reverse xs)
for any finite list xs, where (~+) is define by:
           x ~+ y = y + x
-}
-- If + and a form a monoid, then:
--         foldr (+) a (xs ++ ys) = (foldr (+) a xs) + (foldr (+) a ys)
-- for arbitrary f and a, we have:
--         foldr f a (xs ++ ys) = foldr f (foldr f a xs) ys

-- 3.5.2 Fold over non- e mpty lists
-- foldl1 and foldr1

-- 3.5.3 Scan
-- Apply a fold left operation to every initial segment of a list:
--          scan (+) a [x1,x2,...] = [a, a+x1, (a+x1)+x2,...]

-- 3.5.1
uall :: Foldable t => t Bool -> Bool
uall = foldr (&&) True

-- 3.5.2
-- foldl (-) x xs = x - sum xs

-- 3.5.5
remdups :: (Foldable t, Eq a) => t a -> [a]
remdups xs = foldr cat [] xs
  where
    cat x xs
      | null xs = [x]
      | x == head xs = xs
      | otherwise = x : xs

-- 3.5.6
ssm :: (Foldable t, Ord a) => t a -> [a]
ssm = foldl cat []
  where
    cat xs x
      | null xs = [x]
      | x > last xs = xs ++ [x]
      | otherwise = xs

-- 3.6 List patterns
-- One important distinction between (++) and (:) is that every list can be expressed
-- in terms of [] and (:) in exactly one way. This is not true for concatenation.
-- This special property of (:) means we can do pattern matching with [] and (:).      

-- 3.6.1
-- (n-1) ways

-- 3.6.2
-- xs:[] = [xs] is true. the others are false.

-- 3.6.1
rev2 :: [a] -> [a]
rev2 [x, y] = [y,x]
rev2 x = x

uinsert :: (Foldable t, Ord a) => a -> t a -> [a]
uinsert x = foldr swap [x]
  where 
    swap y [] = [y]
    swap y (x:xs)
      | y > x = x : y : xs 
      | otherwise = y : x : xs 

usort :: Ord a => [a] -> [a]
usort [] = []
usort [x] = [x]
usort (x:xs) = uinsert x (usort xs)