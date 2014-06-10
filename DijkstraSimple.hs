{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Array
import Graph
import Util

type Vertex = Int
newtype Distance = Distance Int deriving (Eq, Ord, Show, Num)

instance Weight Distance where
  infinity = Distance 100000000 
  zero = Distance 0

dijkstra :: (Ix v, Graph a v w) => a -> v -> Array v w
dijkstra g src = array (vbounds g) $ map (\(x,y)->(y, x)) $ dijkstra' initial []
  where dijkstra' [] result = result
        dijkstra' todo result = dijkstra' (map (relax x) xs) (x : result)
          where (x, xs) = let x = minimum todo in (x, x `delete` todo)
                relax (d1, v1) (d2, v2) | d2 < d1 + w = (d2, v2)
                                        | otherwise = (d1 + w, v2) 
                  where w = weight g (v1, v2)
        initial = (zero, src) : [(infinity, x) | x <- (vertices g), x /= src]

main = do (n, edges) <- readGraph 
          let edges' = map (\(x,y,z) -> (x, y, Distance z)) edges
          let g = graph (1 :: Vertex, n :: Vertex) edges :: Dense Vertex Distance
          print $ (dijkstra g (fst . vbounds $ g)) ! (snd . vbounds $ g)
