-- Divide and conquer approach to minfree algorithm
-- minfree returns the smallest Intural number not in a given list.

module MinFree.MinFreeDC (minfree) where
import Data.List (partition)

minfree :: [Int] -> Int
minfree xs = minfrom 0 n (filter (<n) xs)
    where n = (length xs)

minfrom :: Int -> Int -> [Int] -> Int
minfrom a b xs
    | b - a == 0 = error "No answer"
    | b - a == 1 = if length xs == 0 then a else error "No answer"
    | (m-a) > length us = minfrom a m us
    | otherwise = minfrom m b vs
        where
            (us, vs) = (partition (< m) xs)
            m = (a + b) `div` 2