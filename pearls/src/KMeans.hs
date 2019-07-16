module KMeans (kmeans, dist2, computeMeans, computeClasses, initializeMeans)
where
import Data.Array
import Data.List (sortOn)

type Class = Int
type Vector a = [a]
type DataClass a = [(Class, Vector a)]
type Data a = [Vector a]

type DataMean a = Array Class (Vector a)

computeMeans :: (Ord a, Fractional a) => Int -> DataClass a -> DataMean a
computeMeans k = norm . accumArray vAddCnt (zero, 0) (0, k)
    where
        vAdd x y = map (uncurry (+)) $ zip x y
        zero = repeat 0
        vAddCnt (x, n) y = (vAdd x y, n + 1)
        norm = fmap (\(x, n) -> if n > 0 then map (/n) x else x)

computeClasses :: (Ord a, Fractional a) => Int -> Data a -> DataMean a ->
    DataClass a
computeClasses k xs arr = zip (map classify xs) xs
    where
        classify x = fst $ head $ sortOn snd $ assocs $ fmap (dist2 x) arr

dist2 :: (Num a) => Vector a -> Vector a -> a
dist2 x y = sum $ map (^2) $ map (uncurry (-)) $ zip x y

initializeMeans :: Int -> Data a -> DataMean a
initializeMeans k = listArray (0, k)

kmeans :: (Ord a, Fractional a) => Int -> Data a -> [DataClass a]
kmeans k xs= classes
    where
        means = (initializeMeans k xs): map (computeMeans k) classes
        classes = map (computeClasses k xs) means