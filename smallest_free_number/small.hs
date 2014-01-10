module Small where

import Data.Array
import Data.Array.ST
import Data.List (partition)

-- A Natural number ADT, just for fun.
data Nat = Zero | Succ !Nat deriving Eq

instance Show Nat where
    show Zero = "0"
    show (Succ n) = show $ 1 + (read (show n))

instance Num Nat where
    Zero + n = n
    n + Zero = n
    (Succ n) + (Succ m) = Succ . Succ $ n + m

    Zero * n = Zero
    n * Zero = Zero
    (Succ n) * (Succ m) = Succ $ (n + 1) * (m + 1) - 1

    abs n = n

    signum Zero = Zero
    signum _    = Succ Zero

    fromInteger n | n <  0    = error "Nat cannot be negative"
                  | n == 0    = Zero
                  | otherwise = Succ . fromInteger $ n - 1

minfree :: [Int] -> Int
minfree xs = head $ [0..] \\ xs

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (`notElem` vs) us

-- Array version
-- Uses Int's because I'm too lazy to write an Ix instance for Nat
-- and convert other functions to work on Nat.

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) xs'
    where n = length xs
          xs' = zip (filter (<= n) xs) (repeat True)

-- Linear time
countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
    where n = length xs

-- ArrayST monad
checklist' :: [Int] -> Array Int Bool
checklist' xs = runSTArray $ do
    let n = length xs
    a <- newArray (0, n) False
    sequence [writeArray a x True | x <- xs, x <= n]
    return a

-- Divide and Conquer
minfrom :: Int -> [Int] -> Int
minfrom a xs | null xs            = a
             | length us == b - a = minfrom b vs
             | otherwise          = minfrom a us
    where (us, vs) = partition (< b) xs
          b = a + 1 + n `div` 2
          n = length xs

minfree' :: [Int] -> Int
minfree' = minfrom 0

minfree'' :: [Int] -> Int
minfree'' xs = minfrom' 0 (length xs, xs)

minfrom' :: Int -> (Int, [Int]) -> Int
minfrom' a (n, xs) | n == 0     = a
                   | m == b - a = minfrom' b (n - m, vs)
                   | otherwise  = minfrom' a (m,     us)
    where (us, vs) = partition (< b) xs
          b = a + 1 + n `div` 2
          m = length us