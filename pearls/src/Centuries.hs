module Centuries (partitions, centuries, showCenturies) where

type Digit = Int
type Factor = [Digit]
type Term = [Factor]
type Expr = [Term]

partitions :: [a] -> [[[a]]]
partitions [] = [[[]]]
partitions [x] = [[[x]]]
partitions (x:xs) = [[x]:p1:pr | (p1:pr) <- ps] ++ [(x:p1):pr | (p1:pr) <- ps]
    where ps = partitions xs

valFact :: Factor -> Int
valFact = foldl (\c -> \d -> (10 * c + d)) 0

valTerm :: Term -> Int
valTerm = foldr (*) 1 . map valFact

value :: Expr -> Int
value = foldr (+) 0 . map valTerm

centuries n ds = filter ((==n) . value) $ (concatMap partitions . partitions) $
    ds

showCenturies n ds = map showExpr $ centuries n ds

showFact :: Factor -> String
showFact = show . valFact
showTerm :: Term -> String
showTerm = foldl1 (\a -> \b -> a ++ "*" ++ b) . map showFact
showExpr :: Expr -> String
showExpr = foldl1 (\a -> \b -> a ++ " + " ++ b) . map showTerm