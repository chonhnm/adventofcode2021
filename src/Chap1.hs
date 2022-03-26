module Chap1(sign) where

-- 1.1.1
square :: Num a => a -> a
square x = x * x
quad :: Integer -> Integer
quad = square . square
-- 1.1.2
myMax :: Ord p => p -> p -> p
myMax a b =if a >= b then a else b
-- 1.1.3
area :: Floating a => a -> a
area r = pi * square r
-- 1.2.3 
--( succ (pred ( succ (pred (pred zero))))) => (pred zero)
-- from inner to outer
-- (add (succ (pred zero)) zero) => (add zero zero) => zero
-- from outer to inner
-- (add (succ (pred zero)) zero) => (succ (add (pred zero) zero))
-- => (succ (pred (add zero zero))) => (add zero zero) => zero

-- 1.2.4
{-
1110 => 01101 => 0100 => 000 => 00
10 
1110100 => 01001101 => 0110100 => 010000 => 00000 => 0000 => 000 => 00
loop:
110110 => 110 1101 => 1101 1101 => 11101 1101 => 011101 1101 => 101 1101 00(1) 
=> 1101 00 1101 => 1001101 1101 => 1101 1101 1101 => 11101 1101 1101 => 011101 1101 1101
=> 1011101 1101 00  
=> 1101 1101 00 1101 => 1 1101 00 1101 1101 => 01 00 1101 1101 1101 
=> 0 1101 1101 1101 => 01 1101 1101 00 => 101 1101 00 00 =>  1101 0000 1101
=> 1 0000 1101 1101 => 00 1101 1101 => 101 1101 00(2) => loop
-}

-- 1.4.1
integral :: Fractional a => (a -> a) -> a -> a -> a
integral f a b = (f a + f b) / 2 * (b - a)
-- 1.4.2
f1 :: Num t1 => (t1 -> t1) -> t1
f1 f = f 1 
f2 :: Num a => a -> a -> a
f2 x y = x + y 
f3 :: Num a => (a -> a) -> a -> a
f3 f x = f x + 1
-- 1.4.3
sign :: (Ord a, Num a) => a -> a
sign x | x > 0 = 1
       | x < 0 = -1
       | otherwise = 0
-- 1.4.4
one :: Num p1 => p2 -> p1
one x = 1       
apply :: (t1 -> t2) -> t1 -> t2
apply f = f 
compose :: (b -> c) -> (a -> b) -> a -> c 
compose f g x = f (g x)