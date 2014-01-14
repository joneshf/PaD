module Invert where

invertJ :: ((Int, Int) -> Int) -> Int -> [(Int, Int)]
invertJ f z = [(x, y) | x <- [0..z], y <- [0..z], f (x, y) == z]

invertT :: ((Int, Int) -> Int) -> Int -> [(Int, Int)]
invertT f z = [(x, y) | x <- [0..z], y <- [0..z-x], f (x, y) == z]

invertA :: ((Int, Int) -> Int) -> Int -> [(Int, Int)]
invertA f z = findA (0, z) f z

findA :: (Int, Int) -> ((Int, Int) -> Int) -> Int -> [(Int, Int)]
findA (u, v) f z
    | u > z || v < 0 = []
    | z' < z         =        findA (u + 1, v)     f z
    | z' == z        = (u, v):findA (u + 1, v - 1) f z
    | z' > z         =        findA (u,     v - 1) f z
        where z' = f (u, v)

bsearchT :: (Int -> Int) -> (Int, Int) -> Int -> Int
bsearchT g (a, b) z
    | a + 1 == b = a
    | g m <= z   = bsearchT g (m, b) z
    | otherwise  = bsearchT g (a, m) z
        where m = (a + b) `div` 2

invertT' :: ((Int, Int) -> Int) -> Int -> [(Int, Int)]
invertT' f z = findT n (0, m) f z
    where m = bsearchT (\y -> f (0, y)) (-1, z + 1) z
          n = bsearchT (\x -> f (x, 0)) (-1, z + 1) z

findT :: Int -> (Int, Int) -> ((Int, Int) -> Int) -> Int -> [(Int, Int)]
findT n (u, v) f z
    | u == 0 && v == -1 = []
    | u == -1 && v == 0 = []
    | u > n || v < 0    = []
    | z' < z            =        findT n (u + 1, v)     f z
    | z' == z           = (u, v):findT n (u + 1, v - 1) f z
    | z' > z            =        findT n (u,     v - 1) f z
        where z' = f (u, v)

findM :: (Int, Int) -> (Int, Int) -> ((Int, Int) -> Int) -> Int -> [(Int, Int)]
findM (u, v) (r, s) f z
    | u > r || v < s = []
    | v - s <= r - u = rfind (bsearchT (\x -> f (x, q)) (u - 1, r + 1) z)
    | otherwise      = cfind (bsearchT (\y -> f (p, y)) (s - 1, v + 1) z)
        where p = (u + r) `div` 2
              q = (v + s) `div` 2
              rfind p = (if f (p, q) == z
                         then (p, q):findM (u, v) (p - 1, q + 1) f z
                         else        findM (u, v) (p,     q + 1) f z
                        ) ++ findM (p + 1, q - 1) (r, s) f z
              cfind q = findM (u, v) (p - 1, q + 1) f z ++
                        (if f (p, q) == z
                         then (p, q):findM (p + 1, q - 1) (r, s) f z
                         else        findM (p + 1, q)     (r, s) f z)

invertM :: ((Int, Int) -> Int) -> Int -> [(Int, Int)]
invertM f z = findM (0, m) (n, 0) f z
    where m = bsearchT (\y -> f (0, y)) (-1, z + 1) z
          n = bsearchT (\x -> f (x, 0)) (-1, z + 1) z

f0 (x, y) = 2^y * (2*x + 1) - 1
f1 (x, y) = x*2^x + y*2^y + 2*x + y
f2 (x, y) = 3*x + 27*y + y^2
f3 (x, y) = x^2 + y^2 + x + y
f4 (x, y) = x + 2^y + y - 1
