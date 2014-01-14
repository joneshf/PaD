module Smallest where

import Data.Array

smallest :: Ord a => Int -> ([a], [a]) -> a
smallest k (xs, ys) = union (xs, ys) !! k

union :: Ord a => ([a], [a]) -> [a]
union (xs, []) = xs
union ([], ys) = ys
union (x:xs, y:ys)
    | x < y = x:union (xs, y:ys)
    | x > y = y:union (x:xs, ys)

smallest' :: Ord a => Int -> ([a], [a]) -> a
smallest' k ([], ws) = ws !! k
smallest' k (zs, []) = zs !! k
smallest' k (zs, ws) = case (a < b, k <= p + q) of
    (True, True)   -> smallest k           (zs, us)
    (True, False)  -> smallest (k - p - 1) (ys, ws)
    (False, True)  -> smallest k           (xs, ws)
    (False, False) -> smallest (k - q - 1) (zs, vs)
    where p = length zs `div` 2
          q = length ws `div` 2
          (xs, a:ys) = splitAt p zs
          (us, b:vs) = splitAt q ws

smallest'' :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallest'' k (xa, ya) = search k (0, m + 1) (0, n + 1)
    where (0, m) = bounds xa
          (0, n) = bounds ya
          search k' (lx, rx) (ly, ry)
              | lx == rx  = ya ! k'
              | ly == ry  = xa ! k'
              | otherwise = case (xa ! mx < ya ! my, k' <= mx + my) of
                  (True, True)   -> search k'            (lx, rx) (ly, my)
                  (True, False)  -> search (k' - mx - 1) (mx, rx) (ly, ry)
                  (False, True)  -> search k'            (lx, mx) (ly, ry)
                  (False, False) -> search (k' - my - 1) (lx, rx) (my, ry)
                  where mx = (lx + rx) `div` 2
                        my = (ly + ry) `div` 2

list2Array :: [a] -> Array Int a
list2Array xs = listArray (0, length xs - 1) xs
