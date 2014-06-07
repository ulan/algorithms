module Main where

import Data.Array
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as B

type Vertex = Int
type Weight = Int
type Distance = Int 
type Distances = [Distance]
type Mask = [Distance]
type Graph = Array Vertex [Weight]
type Edge = (Vertex, (Vertex, Weight))

infinity = 100000000

graph :: (Vertex, Vertex) -> [Edge] -> Graph
graph (low, high) edges = fmap fillZeroes $ accumArray (flip (:)) [] (low, high) edges
                      where fillZeroes l = elems $ accumArray min infinity (low, high) l

dijkstra_relax :: Graph -> Distances -> (Distance, Vertex) -> Distances 
dijkstra_relax g prev (v_dist, v) = map (\(best, delta) -> min best $ v_dist + delta) $ zip prev (g ! v)

dijkstra_step :: Graph -> (Vertex, Distance, Distances, Mask) -> (Vertex, Distance, Distances, Mask)
dijkstra_step g (v, v_dist, d, m) = (u, u_dist, d', m')
                                where
                                  d' = dijkstra_relax g d (v_dist, v)
                                  (u_dist, u) = minimum $ zip (zipWith max d' m) all
                                  m' = map (\(x, y) -> if (y == u) then infinity else x) $ zip m all
                                  all = range $ bounds g

dijkstra :: Graph -> Distance
dijkstra g = go (s, 0, dists, mask)
              where
                dists = map (\i -> if (i == s) then 0 else infinity) $ range (s, t)
                mask = map (infinity-) dists
                (s, t) = bounds g
                go (v, v_dist, d, m) = if v == t then v_dist else go $ dijkstra_step g (v, v_dist, d, m)

readIntList = (map (fst . fromJust . B.readInt) . (B.split ' ')) <$> B.getLine

main = do [n, m] <- readIntList 
          edges <- replicateM m $ do
            [src, dst, weight] <- readIntList 
            return (src, (dst, weight))
          print $ dijkstra (graph (1, n) edges)
