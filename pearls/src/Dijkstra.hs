module Dijkstra (Graph, (|->), addNode, addEdge, newGraph, fromEdges,
dijkstra)
where
import Data.Map hiding (filter, foldr)
import Data.List (sortOn, nub)

newtype Graph a = G {edgeMap :: Map a [a] }
data Edge a = E a a deriving (Eq, Ord)
(|->) :: a -> a -> Edge a
(|->) = E

fromEdges :: Ord a => [Edge a] -> Graph a
fromEdges = foldr addEdge newGraph

newGraph :: Graph a
newGraph = G empty

addNode :: Ord a =>  a -> Graph a -> Graph a
addNode n = G . insertWith (++) n [] . edgeMap

addEdge :: Ord a => Edge a -> Graph a -> Graph a
addEdge (E s t) = G . adjust (t:) s . edgeMap . addNode s . addNode t

dijkstra :: Ord a => Graph a -> a -> a -> [a]
dijkstra (G g) s t = dijkstraLoop queue paths
    where
        paths = singleton s [s]
        queue = [s]
        dijkstraLoop queue paths = if member t paths
            then reverse $ paths ! t
            else let {
                    newNodes = nub $ filter (not . flip member paths) $ g ! c;
                    c:rest = sortOn (\n -> length (paths ! n)) queue;
                    pathToc = paths ! c;
                    paths' = foldr (\n -> \p -> insert n (n:pathToc) p) paths
                        newNodes;
                    queue' = newNodes ++ rest
                } in dijkstraLoop queue' paths'

instance Show a => Show (Graph a) where
    show = show . assocs . edgeMap