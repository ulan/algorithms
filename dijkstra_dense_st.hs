module Main where

import Data.Array
import Data.Array.ST
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.ST
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

dijkstra :: Graph -> Vertex -> Array Vertex Distance 
dijkstra graph src = runST $ do 
  visited <- newSTArray (bounds graph) False
  distance <- newSTArray (bounds graph) infinity
  writeSTArray distance v1 0
  let next v d i skip d' =
        if skip || d' >= d then (v, d) else (i, d')
  let findMin (v, d) i = 
        do skip <- readSTArray visited i
           d' <- readSTArray distance i
           if i >= vn then return (v, d) else findMin (next v d i skip d') (i + 1)
  let go = do
            (v, d) <- (findMin (v1, infinity) v1)
            let relax (v, d) =
                 do
                  forM_ (graph ! v) $ \(i, j) -> do
                     d' <- readSTArray distance i
                     if d' > d + j then writeSTArray distance i (d + j) else return ()
            writeSTArray visited v True
            if d < infinity then relax (v, d) >> go else return ()
  go
  freeze distance
 where (v1, vn) = bounds graph
       newSTArray :: (Ix i, MArray (STUArray s) e (ST s)) => (i,i) -> e -> ST s (STUArray s i e)
       newSTArray = newArray
       readSTArray :: (Ix i, MArray (STUArray s) e (ST s)) => STUArray s i e -> i -> ST s e
       readSTArray = readArray
       writeSTArray :: (Ix i, MArray (STUArray s) e (ST s)) => STUArray s i e -> i -> e -> ST s () 
       writeSTArray = writeArray

readIntList = (map (fst . fromJust . B.readInt) . (B.split ' ')) <$> B.getLine

main = do [n, m] <- readIntList 
          edges <- replicateM m $ do
            [src, dst, weight] <- readIntList 
            return (src, (dst, weight))
          print $ (dijkstra (graph (1, n) edges) 1) ! n
          
