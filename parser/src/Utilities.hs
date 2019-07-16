module Utilities where

compose :: [a -> a] -> a -> a
compose = foldr (.) id

anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f []     = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]

segments :: [a] -> [([a],[a],[a])]
segments as = [(as1,as2,as3)
              | (as1,bs)  <- splits as,
                (as2,as3) <- splits bs]

splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits (x:xs) = [([],x:xs)]++[(x:ys, zs) | (ys, zs) <- splits xs]

parts :: Int -> [a] -> [[[a]]]
parts 0 [] = [[]]
parts 0 as = []
parts n as = [bs:bss | (bs,cs) <- splits as, bss <- parts (n-1) cs]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (x:xs) = (cp xs) >>= (\ys -> map (\y -> y:ys) x)
