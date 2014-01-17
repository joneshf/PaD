module Ravel where

supravel :: Ord a => [a] -> [[a]]
supravel = minBy length . filter (all up) . unravels

up :: Ord a => [a] -> Bool
up []       = True
up [_]      = True
up (x:y:zs) = x < y && up (y:zs)

minBy :: Ord b => (a -> b) -> [a] -> a
minBy = foldl1 . cmp
    where cmp f u v = if f u <= f v then u else v

unravels :: [a] -> [[[a]]]
unravels = foldr (concatMap . prefixes) [[]]

prefixes :: a -> [[a]] -> [[[a]]]
prefixes x []       = [[[x]]]
prefixes x (xs:xss) = ((x:xs):xss) : map (xs:) (prefixes x xss)

upravels :: Ord a => [a] -> [[[a]]]
--upravels = filter (all up) . unravels
upravels = foldr (concatMap . uprefixes) [[]]

uprefixes :: Ord a => a -> [[a]] -> [[[a]]]
uprefixes x []       = [[[x]]]
uprefixes x (xs:xss) = if x <= head xs
    then ((x:xs):xss):map (xs:) (uprefixes x xss)
    else              map (xs:) (uprefixes x xss)
