module Century where

type Expression = [Term]
type Term = [Factor]
type Factor = [Digit]
type Digit = Int

valExpr :: Expression -> Int
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFact

valFact :: Factor -> Int
valFact = foldl1 (\n d -> 10 * n + d)

good :: Int -> Bool
good = (== 100)

expressions :: Factor -> [Expression]
-- This would work if `paritions` where somewhere to be found.
--expressions = concatMap partitions . partitions
expressions = foldr extend []

extend :: Digit -> [Expression] -> [Expression]
extend x [] = [[[[x]]]]
extend x es = concatMap (glue x) es

-- Don't fully grok tis one.  Might be still too sick...
glue :: Digit -> Expression -> [Expression]
glue x ((xs:xss):xsss) = [ ((x:xs):xss):xsss
                         , ([x]:xs:xss):xsss
                         , [[x]]:(xs:xss):xsss
                         ]

value :: Expression -> (Int, Int, Int, Int)
value ((xs:xss):xsss) = (10 ^ n, valFact xs, valTerm xss, valExpr xsss)
    where n = length xs

modify :: Int -> (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
modify x (k, f, t, e) = [ (10 * k, k * x + f, t, e)
                        , (10, x, f * t, e)
                        , (10, x, 1, f * t + e)
                        ]

good', ok :: Int -> (Int, Int, Int, Int) -> Bool
good' c (_, f, t, e) = f * t + e == c
ok    c (_, f, t, e) = f * t + e <= c

solutions :: Digit -> Factor -> [Expression]
solutions c = map fst . filter (good' c . snd) . foldr (expand c) []

expand :: Digit
       -> Digit
       -> [(Expression, (Digit, Digit, Digit, Digit))]
       -> [(Expression, (Digit, Digit, Digit, Digit))]
expand _ x []  = [([[[x]]], (10, x, 1, 0))]
expand c x evs = concatMap (filter (ok c . snd) . glue' x) evs

glue' :: Digit
      -> (Expression, (Digit, Digit, Digit, Digit))
      -> [(Expression, (Digit, Digit, Digit, Digit))]
glue' x ((xs:xss):xsss, (k, f, t, e)) =
    [ (((x:xs):xss):xsss, (10 * k, k * x + f, t, e))
    , (([x]:xs:xss):xsss, (10, x, f * t, e))
    , ([[x]]:(xs:xss):xsss, (10, x, 1, f * t + e))
    ]
