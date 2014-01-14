module Sort where

import Data.Array
import Data.List

type Label a = (a, (Int, Int))

sortsums :: (Num a, Ord a) => [a] -> [a] -> [a]
sortsums xs ys = sort [x + y | x <- xs, y <- ys]

subs :: Num a => [a] -> [a] -> [Label a]
subs xs ys = [(x - y, (i, j)) | (x, i) <- zip xs [1..], (y, j) <- zip ys [1..]]

sortsums' :: (Num a, Ord a) => [a] -> [a] -> [a]
sortsums' xs = map fst . sortsubs xs . map negate

sortsubs :: (Num a, Ord a) => [a] -> [a] -> [Label a]
sortsubs xs = sort . subs xs

table :: (Num a, Ord a) => [a] -> [a] -> [(Int, Int, Int)]
table xs ys = map (snd . tag 1) xxs `merge` map (snd . tag 2) yys
    where xxs = sortsubs xs xs
          yys = sortsubs ys ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x < y = x:merge xs (y:ys)
    | x > y = y:merge (x:xs) ys
    | otherwise = x:y:merge xs ys

tag :: b -> (a, (c, d)) -> (a, (b, c, d))
tag i (x, (j, k)) = (x, (i, j, k))

mkArray :: (Enum a, Num a, Ord a) => [a] -> [a] -> Array (Int, Int, Int) a
mkArray xs ys = array b (zip (table xs ys) [1..])
    where b = ((1, 1, 1), (2, p, p))
          p = max (length xs) (length ys)

sortsubs' :: (Enum a, Num a, Ord a) => [a] -> [a] -> [Label a]
sortsubs' xs ys = sortBy (cmp (mkArray xs ys)) (subs xs ys)

cmp :: Ord a
    => Array (Int, Int, Int) a
    -> (a, (Int, Int))
    -> (a, (Int, Int))
    -> Ordering
cmp a (_, (i, j)) (_, (k, l)) = compare (a ! (1, i, k)) (a ! (2, j, l))

sortsums'' :: (Enum a, Num a, Ord a) => [a] -> [a] -> [a]
sortsums'' xs = map fst . sortsubs' xs . map negate

table' :: (Enum a, Num a, Ord a) => [a] -> [a] -> [(Int, Int, Int)]
table' xs ys = map (snd . tag 1) xxs `merge` map (snd . tag 2) yys
    where xxs = sortsubs'' xs
          yys = sortsubs'' ys

sortsubs'' :: (Enum a, Num a, Ord a) => [a] -> [Label a]
sortsubs'' xs = sortsubs' xs xs

-- Quadratic number of comparisons.
sortsubs''' :: (Enum a, Num a, Ord a) => [a] -> [Label a]
sortsubs''' []  = []
sortsubs''' [w] = [(w - w, (1, 1))]
sortsubs''' ws  = foldr1 merge [ xxs
                               , map (incr m) xys
                               , map (incl m) yxs
                               , map (incb m) yys
                               ]
    where xxs = sortsubs''' xs
          xys = sortBy (cmp (mkArray xs ys)) (subs xs ys)
          yxs = map switch (reverse xys)
          yys = sortsubs''' ys
          (xs, ys) = splitAt m ws
          m = length ws `div` 2

incl, incr, incb :: Num a => a -> (b, (a, a)) -> (b, (a, a))
incl m (x, (i, j)) = (x, (m + i, j))
incr m (x, (i, j)) = (x, (i, m + j))
incb m (x, (i, j)) = (x, (m + i, m + j))

switch :: Num a => (a, (b, c)) -> (a, (c, b))
switch (x, (i, j)) = (negate x, (j, i))
