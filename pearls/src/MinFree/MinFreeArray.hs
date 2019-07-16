-- array approach to minfree algorithm
-- minfree returns the smallest Intural number not in a given list.

module MinFree.MinFreeArray (minfree) where
import Data.Array

minfree :: [Int] -> Int
minfree xs = (search . rollcall . filter (<n)) xs
    where n = length xs
rollcall :: [Int] -> Array Int Bool
rollcall xs = accumArray (&&) True (0, length xs) $ zip xs $ repeat False
search :: Array Int Bool -> Int
search = head . map fst . filter snd . assocs