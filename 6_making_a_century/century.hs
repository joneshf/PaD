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
