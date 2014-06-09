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
type Graph = Array (Vertex, Vertex) Weight
type Edge = ((Vertex, Vertex), Weight)

infinity = 100000000

graph :: (Vertex, Vertex) -> [Edge] -> Graph
graph (v1, vn) = accumArray min infinity ((v1, v1), (vn, vn))

dijkstra :: Graph -> Vertex -> Array Vertex Distance
dijkstra graph src = array (v1, vn) $ map (\(x,y)->(y, x)) $ dijkstra' initial []
  where dijkstra' [] result = result
        dijkstra' xs result = dijkstra' (map (relax x) xs') (x : result)
          where (x, xs') = let x = minimum xs in (x, x `delete` xs)
                relax (cd, cv) (d, v) | d < cd + graph ! (cv, v) = (d, v)
                                      | otherwise = (cd + graph ! (cv, v), v) 
        initial = ((0, src) : [(infinity, x) | x <- range (v1, vn), x /= src])
        ((v1, _), (vn, _)) = bounds graph

readIntList = (map (fst . fromJust . B.readInt) . (B.split ' ')) <$> B.getLine

main = do [n, m] <- readIntList 
          edges <- replicateM m $ do
            [src, dst, weight] <- readIntList 
            return ((src, dst), weight)
          print $ (dijkstra (graph (1, n) edges) 1) ! n
          
