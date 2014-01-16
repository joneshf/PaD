module Height where

data Tree = Leaf Int | Fork Tree Tree
    deriving Show
type Forest = [Tree]

ints2TreeTD :: [Int] -> Tree
ints2TreeTD [x] = Leaf x
ints2TreeTD xs  = Fork (ints2TreeTD front) (ints2TreeTD back)
    where (front, back) = splitAt middle xs
          middle = length xs `div` 2

trees :: [Int] -> Forest
trees [x]    = [Leaf x]
trees (x:xs) = concatMap (prefixes x) (trees xs)

prefixes :: Int -> Tree -> Forest
prefixes x l@(Leaf _)   = [Fork (Leaf x) l]
prefixes x l@(Fork y z) = Fork (Leaf x) l : [Fork y' z | y' <- prefixes x y]

foldrn :: (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn _ g [x]    = g x
foldrn f g (x:xs) = f x (foldrn f g xs)

trees' :: [Int] -> Forest
trees' = foldrn (concatMap . prefixes) (return . Leaf)

trees'' :: [Int] -> Forest
trees'' = map rollup . forests

forests :: [Int] -> [Forest]
forests = foldrn (concatMap . prefixes') (return . return . Leaf)

prefixes' :: Int -> Forest -> [Forest]
prefixes' x ts = [Leaf x : rollup (take k ts) : drop k ts | k <- [1..length ts]]

rollup :: Forest -> Tree
rollup = foldr1 Fork

minBy :: Ord b => (a -> b) -> [a] -> a
minBy = foldl1 . cmp

cmp :: Ord b => (a -> b) -> a -> a -> a
cmp f u v = if f u <= f v then u else v
