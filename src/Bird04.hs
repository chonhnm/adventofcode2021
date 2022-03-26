--{-# LANGUAGE NoImplicitPrelude #-}

module Bird04 where

import Control.Arrow (Arrow (second))
import Prelude hiding (Word, lines, unlines, unwords, words)

--import Protolude hiding (link, lines, Text, unlines, words,unwords)

-- 4.1 Converting numbers to words

units :: [[Char]]
units = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: [[Char]]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: [[Char]]
tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

convert2 :: Int -> [Char]
convert2 n = combine2 (digit2 n)

digit2 :: Integral b => b -> (b, b)
digit2 n = (n `div` 10, n `mod` 10)

combine2 :: (Int, Int) -> [Char]
combine2 (0, y) = units !! y
combine2 (1, y) = teens !! y
combine2 (x, 0) = tens !! x
combine2 (x, y) = tens !! x ++ "-" ++ units !! y

convert3 :: Int -> [Char]
convert3 n = combine3 (digit3 n)

digit3 :: Integral b => b -> (b, b)
digit3 n = (n `div` 100, n `mod` 100)

combine3 :: (Int, Int) -> [Char]
combine3 (0, y) = convert2 y
combine3 (x, 0) = units !! x ++ " hundred"
combine3 (x, y) = units !! x ++ " hundred and " ++ convert2 y

convert6 :: Int -> [Char]
convert6 n = combine6 (digit6 n)

digit6 :: Integral b => b -> (b, b)
digit6 n = (n `div` 1000, n `mod` 1000)

combine6 :: (Int, Int) -> [Char]
combine6 (0, y) = convert3 y
combine6 (x, 0) = convert3 x ++ " thousand"
combine6 (x, y) = convert3 x ++ " thousand" ++ link y ++ convert3 y

link :: (Ord a, Num a) => a -> [Char]
link h
  | h < 100 = " and "
  | otherwise = " "

convert9 :: Int -> [Char]
convert9 n = combine9 (digit9 n)

digit9 :: Integral b => b -> (b, b)
digit9 n = (n `div` 1000000, n `mod` 1000000)

combine9 :: (Int, Int) -> [Char]
combine9 (0, y) = convert6 y
combine9 (x, 0) = convert3 x ++ " million"
combine9 (x, y) = convert3 x ++ " million" ++ link (y `div` 1000) ++ convert6 y

convert :: Int -> [Char]
convert n
  | n > 0 = convert9 n ++ "."
  | n < 0 = "negative " ++ convert9 (negate n) ++ "."
  | otherwise = "0"

-- convert number to RMB
cunits :: [[Char]]
cunits = ["", "壹", "贰", "叁", "肆", "伍", "陆", "柒", "捌", "玖"]

cteens :: [[Char]]
cteens = ["拾", "拾壹", "拾贰", "拾叁", "拾肆", "拾伍", "拾陆", "拾柒", "拾捌", "拾玖"]

ctens :: [[Char]]
ctens = ["", "", "贰拾", "叁拾", "肆拾", "伍拾", "陆拾", "柒拾", "捌拾", "玖拾"]

-- convert most of 2 digit
cconvert2 :: Int -> [Char]
cconvert2 n = ccombine2 (cdigit2 n)

cdigit2 :: Integral b => b -> (b, b)
cdigit2 n = (n `div` 10, n `mod` 10)

ccombine2 :: (Int, Int) -> [Char]
ccombine2 (0, y) = cunits !! y
ccombine2 (1, y) = cteens !! y
ccombine2 (x, 0) = ctens !! x
ccombine2 (x, y) = ctens !! x ++ cunits !! y

cconvert3 :: Int -> [Char]
cconvert3 n = ccombine3 (cdigit3 n)

cdigit3 :: Integral b => b -> (b, b)
cdigit3 n = (n `div` 100, n `mod` 100)

ccombine3 :: (Int, Int) -> [Char]
ccombine3 (0, y) = cconvert2 y
ccombine3 (x, 0) = cunits !! x ++ "佰"
ccombine3 (x, y) = cunits !! x ++ "佰" ++ clink (y `div` 10) ++ cconvert2 y

clink :: (Eq a, Num a) => a -> [Char]
clink n
  | n == 0 = "零"
  | otherwise = ""

cconvert4 :: Int -> [Char]
cconvert4 n = ccombine4 (cdigit4 n)

cdigit4 :: Integral b => b -> (b, b)
cdigit4 n = (n `div` 1000, n `mod` 1000)

ccombine4 :: (Int, Int) -> [Char]
ccombine4 (0, y) = cconvert3 y
ccombine4 (x, 0) = cunits !! x ++ "仟"
ccombine4 (x, y) = cunits !! x ++ "仟" ++ clink (y `div` 100) ++ cconvert3 y

cconvert8 :: Int -> [Char]
cconvert8 n = ccombine8 (cdigit8 n)

cdigit8 :: Integral b => b -> (b, b)
cdigit8 n = (n `div` 10000, n `mod` 10000)

ccombine8 :: (Int, Int) -> [Char]
ccombine8 (0, y) = cconvert4 y
ccombine8 (x, 0) = cconvert4 x ++ "万"
ccombine8 (x, y) = cconvert4 x ++ "万" ++ clink (y `div` 1000) ++ cconvert4 y

cconvert16 :: Int -> [Char]
cconvert16 n = ccombine16 (cdigit16 n)

cdigit16 :: Integral b => b -> (b, b)
cdigit16 n = (n `div` 100000000, n `mod` 100000000)

ccombine16 :: (Int, Int) -> [Char]
ccombine16 (0, y) = cconvert8 y
ccombine16 (x, 0) = cconvert8 x ++ "亿"
ccombine16 (x, y) = cconvert8 x ++ "亿" ++ clink (y `div` 10000000) ++ cconvert8 y

cconvert :: Int -> [Char]
cconvert n = cconvert16 n ++ "圆整"

--------------------
-- 4.2 Variable-length arithmetic
-------------------
type VInt = [Bigit]

type Bigit = Int

b :: Bigit
b = 10

strep :: VInt -> VInt
strep xs
  | null ys = [0]
  | otherwise = ys
  where
    ys = dropWhile (== 0) xs

align :: VInt -> VInt -> (VInt, VInt)
align xs ys
  | n > 0 = (replicate n 0 ++ xs, ys)
  | otherwise = (xs, replicate (- n) 0 ++ ys)
  where
    n = length ys - length xs

vcompare :: (VInt -> VInt -> t) -> [Bigit] -> [Bigit] -> t
vcompare op xs ys = op us vs where (us, vs) = align xs ys

veq :: [Bigit] -> [Bigit] -> Bool
veq = vcompare (==)

vleq :: [Bigit] -> [Bigit] -> Bool
vleq = vcompare (<=)

vless :: [Bigit] -> [Bigit] -> Bool
vless = vcompare (<)

vlargeq :: [Bigit] -> [Bigit] -> Bool
vlargeq = vcompare (>=)

vlarge :: [Bigit] -> [Bigit] -> Bool
vlarge = vcompare (>)

carry :: Bigit -> [Bigit] -> [Bigit]
carry x (c : xs) = (x + c) `div` b : (x + c) `mod` b : xs
carry _ _ = error "err"

norm :: VInt -> VInt
norm = strep . foldr carry [0]

vadd :: VInt -> VInt -> VInt
vadd xs ys = norm (zipWith (+) us vs) where (us, vs) = align xs ys

vsub :: [Bigit] -> [Bigit] -> VInt
vsub xs ys = norm (zipWith (-) us vs) where (us, vs) = align xs ys

negative :: VInt -> Bool
negative xs = head xs < 0

vnegate :: [Bigit] -> VInt
vnegate = norm . map neg

neg :: Num a => a -> a
neg x = - x

-- Multiplication
psums :: [Bigit] -> [Bigit] -> [VInt]
psums xs = map (bmul xs)

bmul :: [Bigit] -> Bigit -> VInt
bmul xs y = norm (map (* y) xs)

vmul :: [Bigit] -> [Bigit] -> VInt
vmul xs ys = foldl1 sadd (psums xs ys)
  where
    sadd xs ys = vadd (xs ++ [0]) ys

divalg :: [Bigit] -> [Bigit] -> [(Bigit, [Bigit])]
divalg xs ys = scanl (dstep ys) (0, take m xs) (drop m xs)
  where
    m = length ys - 1

dstep :: [Bigit] -> (a, [Bigit]) -> Int -> (Bigit, [Bigit])
dstep ys (q, rs) x
  | xlen < ylen = astep xs ys
  | xlen == ylen = bstep xs ys
  | xlen == ylen + 1 = cstep xs ys
  | otherwise = error "error." -- we must ensure the first bigit y0 of y is bigger than b / 2
  where
    xs = rs ++ [x]
    xlen = length xs
    ylen = length ys

astep :: Num a => b -> p -> (a, b)
astep xs ys = (0, xs)

bstep :: Num a => VInt -> [Bigit] -> (a, VInt)
bstep xs ys
  | negative zs = (0, xs)
  | otherwise = (1, zs)
  where
    zs = vsub xs ys

cstep :: [Bigit] -> [Bigit] -> (Bigit, VInt)
cstep xs ys
  | vless rs0 ys = (q, rs0)
  | vless rs1 ys = (q + 1, rs1)
  | otherwise = (q + 2, rs2)
  where
    rs0 = vsub xs (bmul ys q)
    rs1 = vsub rs0 ys
    rs2 = vsub rs1 ys
    q = guess xs ys - 2

guess :: [Bigit] -> [Bigit] -> Bigit
guess (x0 : x1 : xs) (y1 : ys)
  | x0 >= y1 = b - 1
  | otherwise = (x0 * b + x1) `div` y1
guess _ _ = error "ee"

vqrm :: [Bigit] -> [Bigit] -> (VInt, VInt)
vqrm xs ys = (strep qs, strep rs)
  where
    qs = map fst ds
    rs = bdiv (snd (last ds)) d
    ds = divalg (bmul xs d) (bmul ys d)
    d = b `div` (head ys + 1)

bqrm :: [Bigit] -> Bigit -> (VInt, Bigit)
bqrm (x : xs) d = (strep qs, last rs `mod` d)
  where
    qs = map (`div` d) rs
    rs = scanl br x xs
    br r x = b * (r `mod` d) + x
bqrm _ _ = error "error."

bdiv :: [Bigit] -> Bigit -> VInt
bdiv xs d = fst $ bqrm xs d

bmod :: [Bigit] -> Bigit -> Bigit
bmod xs d = snd $ bqrm xs d

-----------------------
--- 4.3 Text processing
-----------------------
-- type Text = [Char]

-- type Line = [Char]

-- lines :: Text -> [Line]
-- lines = foldr f [[]]
--   where
--     f x xss
--       | x == '\n' = [] : xss
--       | otherwise = (x : head xss) : tail xss

-- unlines :: [Line] -> Text
-- unlines = foldr1 (\xs ys -> xs ++ "\n" ++ ys)

-- type Word = [Char]

-- words :: Line -> [Word ]
-- words = filter (/= []) . foldr f [[]]
--   where
--     f x xss
--       | x == ' ' = [] : xss
--       | otherwise = (x : head xss) : tail xss

-- unwords :: [Word] -> Line
-- unwords = foldr1 (\xs ys -> xs ++ [' '] ++ ys)

type Text = [Char]

type Line = [Char]

type Word = [Char]

type Para = [Line]

unlines :: [Line] -> Text
unlines = foldr1 (ins '\n')

unwords :: [Word] -> Line
unwords = foldr1 (ins ' ')

unparas :: [Para] -> [Line]
unparas = foldr1 (ins [])

ins :: a -> [a] -> [a] -> [a]
ins a xs ys = xs ++ [a] ++ ys

lines :: Text -> [Line]
lines = foldr (breakon '\n') [[]]

words :: Line -> [Word]
words = filter (/= []) . foldr (breakon ' ') [[]]

paras :: [Line] -> [Para]
paras = filter (/= []) . foldr (breakon []) [[]]

breakon :: Eq a => a -> a -> [[a]] -> [[a]]
breakon a x xss
  | x == a = [] : xss
  | otherwise = (x : head xss) : tail xss

countlines :: Text -> Int
countlines = length . lines

countwords :: Text -> Int
countwords = length . concatMap words . lines

countparas :: Text -> Int
countparas = length . paras . lines

normalise :: Text -> Text
normalise = unpars . pars

pars :: Text -> [[[Word]]]
pars = map (map words) . paras . lines

unpars :: [[[Word]]] -> Text
unpars = unlines . unparas . map (map unwords)

fill :: Int -> [Word] -> [[Word]]
fill m ws
  | null ws = []
  | otherwise = fstline : fill m restwds
  where
    fstline = take n ws
    restwds = drop n ws
    n = greedy m ws

greedy :: Int -> [Word] -> Int
greedy m ws = length (takeWhile (<= m) (scanl f (-1) ws)) - 1
  where
    f n word = n + length word + 1

filltext :: Int -> Text -> Text
filltext m = unpars . map (fill m) . textparas 

textparas :: Text -> [[Word]]
textparas = map linewords . paras . lines 

linewords :: [Line] -> [Word]
linewords = concatMap words 