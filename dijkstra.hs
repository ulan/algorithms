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
type Distance = Int 
type Graph = Array Vertex [(Vertex, Weight)]
type Edge = (Vertex, (Vertex, Weight))

infinity = 100000000

graph :: (Vertex, Vertex) -> [Edge] -> Graph
graph (min, max) edges = accumArray (flip (:)) [] (min, max) edges

data Dijkstra = Dijkstra (PSQ Vertex Distance) [(Vertex, Distance)]

dijkstra :: Graph -> Vertex -> [(Vertex, Distance)]
dijkstra graph src = go $ Dijkstra (adjust (\_ -> 0) src initial) []
  where initial = fromList [(i :-> infinity) | i <- range (bounds graph)]
        go (Dijkstra pq dist) = case minView pq of
	    Nothing -> dist 
	    Just ((k :-> d), pq') -> go $ Dijkstra (update pq' k d) ((k, d) : dist)
        update pq k d = foldl' relax pq [(v, d + w) | (v, w) <- graph ! k]
        relax pq (v, d') = adjust (\d -> min d d') v pq

readIntList = (map (fst . fromJust . B.readInt) . (B.split ' ')) <$> B.getLine

main = do [n, m] <- readIntList 
          edges <- replicateM m $ do
            [src, dst, weight] <- readIntList 
            return (src, (dst, weight))
          print $ fromJust $ Data.List.lookup n (dijkstra (graph (1, n) edges) 1)
          
