module KSmallUnion (smallest, smallestAr) where
import Data.Array
import Debug.Trace (trace)
import Text.Printf

smallest :: (PrintfArg a, Ord a) => Int -> ([a], [a]) -> a
smallest k (xs, ys) = smallestAr k (xa, ya)
    where
        xa = listArray (0, length xs - 1) xs
        ya = listArray (0, length ys - 1) ys

-- * KSmallUnion.hs lines 11-29
smallestAr :: (PrintfArg a, Ord a) => Int -> (Array Int a, Array Int a) -> a
smallestAr k (xa, ya) = search k (0, m+1) (0, n+1)
    where
        (0, m) = bounds xa
        (0, n) = bounds ya
        search k (lx, rx) (ly, ry)
            | lx == rx = ya ! k
            | ly == ry = xa ! k
            | otherwise = trace summary
                (case (a < b, k <= (mx) + (my)) of
                    (True, True) -> search k (lx, rx) (ly, my)
                    (True, False) -> search (k-mx-1) (mx+1, rx) (ly, ry)
                    (False, True) -> search k (lx, mx) (ly, ry)
                    (False, False) -> search (k-my-1) (lx, rx) (my+1, ry))
            where
                mx = (lx + rx) `div` 2
                my = (ly + ry) `div` 2
                a = xa ! mx
                b = ya ! my
                summary = (printf ("k: %d (lx: %d, mx: %d, a: %d, rx: %d)"
                    ++" (ly: %d, my: %d, b: %d, ry: %d)") k lx mx a rx ly my b ry)
