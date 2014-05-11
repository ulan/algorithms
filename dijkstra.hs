module Main where

import Data.Array
import Data.List
import Data.Maybe
import Data.PSQueue
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as B


type Vertex = Int
type Weight = Int
type Graph = Array Vertex [(Vertex, Weight)]
type Edge = (Vertex, (Vertex, Weight))
type Distance = (Vertex, Weight)

infinity = 100000000

graph :: (Vertex, Vertex) -> [Edge] -> Graph
graph (min, max) edges = accumArray (flip (:)) [] (min, max) edges

data Dijkstra = Dijkstra (PSQ Vertex Weight) [Distance]

dijkstra :: Graph -> Vertex -> [Distance]
dijkstra graph src = go $ Dijkstra (adjust (\_ -> 0) src initial) []
  where initial = fromList [(i :-> infinity) | i <- range (bounds graph)]
        go (Dijkstra pq dist) = case minView pq of
	    Nothing -> dist 
	    Just ((v :-> w), pq') -> go $ Dijkstra (update pq' v w) ((v, w) : dist)
        update pq v w = foldl' relax pq [(nv, w + nw) | (nv, nw) <- graph ! v]
        relax pq (nv, nw) = adjust (\w -> min w nw) nv pq

readIntList = (map (fst . fromJust . B.readInt) . (B.split ' ')) <$> B.getLine

main = do [n, m] <- readIntList 
          edges <- replicateM m $ do
            [src, dst, weight] <- readIntList 
            return (src, (dst, weight))
          print $ Data.List.lookup n (dijkstra (graph (1, n) edges) 1)
          
