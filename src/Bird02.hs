module Bird02 (t11) where

import Chap1 (sign)
import Data.Char

mySqrt :: (Fractional p, Ord p) => p -> p
mySqrt x = until satis improve x
  where
    satis y = abs (y ^ 2 - x) < eps
    improve y = (y + x / y) / 2
    eps = 0.00001

-- Newton's method: if y is an approximation to a root of a function f, then:
-- y - f(y)/f'(y) is a better approximation
deriv :: Fractional a => (a -> a) -> a -> a
deriv f x = (f (x + dx) - f x) / dx where dx = 0.00001

newton :: (Fractional p, Ord p) => (p -> p) -> p -> p
newton f = until satis improve
  where
    satis y = abs (f y) < eps
    improve y = y - (f y / deriv f y)
    eps = 1.0e-5

mySqrt2 :: (Fractional p, Ord p) => p -> p
mySqrt2 x = newton f x where f y = y ^ 2 - x

cubrt :: (Fractional p, Ord p) => p -> p
cubrt x = newton f x where f y = y ^ 3 - x

-- say: q = (x div y), r = (x mod y), y>0. we have: x = q * y + r and 0 <= r < y

-- 2.1.5
t1 :: Integer -> Bool
t1 = (== 9) . (2 +) . (7 *)

t11 :: Bool
t11 = t1 1

t2 :: Integer -> Bool
t2 = (3 >) . (`mod` 2)

t22 :: Bool
t22 = t2 9

-- 2.1.7
mySqrt12 :: (Fractional p, Ord p) => p -> p
mySqrt12 x = until satis improve x
  where
    satis y = abs (y ^ 2 - x) < eps * x
    improve y = (y + x / y) / 2
    eps = 0.00001

mySqrt13 :: (Fractional p, Ord p) => p -> p
mySqrt13 x = until satis improve x
  where
    satis y = abs (y - improve y) < eps * abs y
    improve y = (y + x / y) / 2
    eps = 0.00001

{-
********************
2.2 Booleans
********************
-}
within :: (Ord a, Num a) => a -> a -> a -> Bool
within eps x y = abs (x - y) < eps

leap :: Integral a => a -> Bool
leap y =
  if y `mod` 100 == 0
    then y `mod` 400 == 0
    else y `mod` 4 == 0

-- 2.2.2
sumsqrs :: (Num a, Ord a) => a -> a -> a -> a
sumsqrs a b c
  | a <= b && a <= c = b ^ 2 + c ^ 2
  | b <= a && b <= c = a ^ 2 + c ^ 2
  | c <= a && c <= b = a ^ 2 + b ^ 2
  | otherwise = error "error."

{-
********************
2.3 Characters and strings
********************
-}
space :: (Num t, Enum t) => t -> [Char]
space n = [' ' | t <- [1 .. n]]

ljustify :: Int -> [Char] -> [Char]
ljustify n x
  | n >= m = x ++ space (n - m)
  | otherwise = x
  where
    m = length x

rjustify :: Int -> [Char] -> [Char]
rjustify n x
  | n >= m = space (n - m) ++ x
  | otherwise = x
  where
    m = length x

cjustify :: Int -> [Char] -> [Char]
cjustify n x
  | n >= m = space lm ++ x ++ space rm
  | otherwise = x
  where
    m = length x
    lm = (n - m) `div` 2
    rm = (n - m) - lm

-- 2.3.1
nextlet :: Char -> Char
nextlet c = chr $ ord c + 1

digitval :: Char -> Int
digitval c
  | isDigit c = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise = error $ "not a digit: " ++ show c

{-
********************
2.4 Tuples
********************
-}

radd :: Integral a => (a, a) -> (a, a) -> (a, a)
radd (x, y) (u, v) = norm (x * v + u * y, y * v)

rsub :: Integral a => (a, a) -> (a, a) -> (a, a)
rsub (x, y) (u, v) = norm (x * v - u * y, y * v)

rmul :: Integral b => (b, b) -> (b, b) -> (b, b)
rmul (x, y) (u, v) = norm (x * u, y * v)

rdiv :: Integral b => (b, b) -> (b, b) -> (b, b)
rdiv (x, y) (u, v) = norm (x * v, y * u)

norm :: Integral b => (b, b) -> (b, b)
norm (x, y)
  | y /= 0 = (div u d, div v d)
  | otherwise = error "y cannot be zero."
  where
    u = sign y * x
    v = abs y
    d = gcd (abs u) v

rcompare :: Integral t => (t -> t -> Bool) -> (t, t) -> (t, t) -> Bool
rcompare op (x, y) (u, v) = op (x * v) (y * u)

requals :: Integral a => (a, a) -> (a, a) -> Bool
requals = rcompare (==)

rless :: Integral a => (a, a) -> (a, a) -> Bool
rless = rcompare (<)

rgreater :: Integral a => (a, a) -> (a, a) -> Bool
rgreater = rcompare (>)

showrat :: (Show a, Integral a) => (a, a) -> String
showrat (x, y)
  | v == 1 = show u
  | otherwise = show u ++ "/" ++ show v
  where
    (u, v) = norm (x, y)

k :: (Integer, Integer)
k = rmul (4, 5) (5, 8)

{-
********************
2.5 Patterns
********************
-}
upred :: (Eq p, Integral p) => p -> p
upred 0 = 0
upred n = n - 1

{-
********************
2.6 Functions
********************
-}

{-
One good test of whether to define a function as an operator i s to see i f the
operation is associative. Since the rational addition operator + is associative,
it is more pleasant to be able to write: x + y + z
-}
three :: Num p1 => p2 -> p1
three x = 3

j :: Integer
j = three (1 / 0)

g :: a -> b -> Int
g a b = 1

f :: Int -> Bool
f _ = True

kk = f . g 1

kkk = g abs

{-
********************
2.7 Type synonyms
********************
-}
type Pos = (Integer, Integer)

type Pairs a = (a, a)

type Automorph a = a -> a

{-
********************
2.8 Type inference
********************
-}
-- Three rules:
-- Application rule: If f x :: t, then x :: t' and f :: t' -> t for some new type t'.
-- Equality rule: If both the types x::t and x::t' can be deduced for a variable x, then t = t'.
-- Function rule: If t -> u = t' -> u', then t = t' and u = u'.
{-
for: (.) f g x = f (g x)
first, we have:
f :: t1
g :: t2
x :: t3
f (g x) :: t4
then, we use three rules to analyse the defining expression f (g x)
Using the application rule on f (g x) :: t4, we duduce that:
g x :: t5
f :: t5 -> t4
Similarly, from g x :: t5 we deduce that:
x :: t6
g :: t6 -> t5
Using the equality rule, we can now obtain the following identities:
t1 = t5 -> t4
t2 = t6 -> t5
t3 = t6
Then, the type we can deduce for (.) is:
(.):: (t5->t4)->(t6->t5)->t6->t4
Finally, we can replace the type names by generic type variables, giving:
(.) :: (b->c) -> (a->b) -> a -> c
-}

-- 2.8.1
uconst :: p1 -> p2 -> p1
uconst x y = x 
{-
subst f g x = f x (g x)
First:
f :: t1
g :: t2
x :: t3
f x (g x) :: t4
Then, use applicative rule:
g x :: t5
f x :: t5 -> t4
x :: t6
g :: t6 -> t5
x :: t7
f :: t7 -> t5 -> t4
Use function rule and equality rule, we have:
t3 = t6 = t7
t2 = t6 -> t5
t1 = t7 -> t5 -> t4
then:
subst :: (t3 -> t5 -> t4) -> (t3 -> t5) -> t3 -> t4
Last, generic:
subst :: (a -> b -> c) -> (a -> b) -> a -> c
-}
subst :: (t1 -> t2 -> t3) -> (t1 -> t2) -> t1 -> t3
subst f g x = f x (g x)
{-
fix f x = f (fix f) x 
First:
f :: t1
x :: t2
f (fix f) x :: t3
fix :: t1 -> t2 -> t3
Then, use applicative rule:
x :: t4
f (fix f) :: t4 -> t3
fix f :: t5
f :: t5 -> t4 -> t3
f :: t6
fix :: t6 -> t5
Use function rule and equality rule, we have:
t1 = t6 = t5 -> t4 -> t3
t2 = t4
t5 = t2 -> t3
Then:
fix :: ((t2->t3)->t2->t3) -> t2 -> t3
Generic:
fix :: ((t1->t2)->t1->t2) -> t1 -> t2
-}
ufix :: ((t1 -> t2) -> t1 -> t2) -> t1 -> t2
ufix f x = f (ufix f) x 

--2.8.2
uid :: p1 -> p1
uid = subst uconst uconst
{-
uid x = uconst x (uconst x) = x 
-}
ucompose :: (t2 -> t3) -> (b -> t2) -> b -> t3
ucompose f = subst (const f) 

-- 2.8.3
uapply :: (t1 -> t2) -> t1 -> t2
uapply f x = f x 

-- 2.8.4
query :: (t -> t -> t) -> t -> t -> t
query f x g = f g (f x g)