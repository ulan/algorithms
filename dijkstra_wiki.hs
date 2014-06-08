import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Data.Set as S

import Data.List
import Data.Maybe
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

dijkstra :: (Ix v, Num w, Ord w, Bounded w) => v -> Array v [(v,w)] -> Array v w
dijkstra src adj_list = runST $ do
  min_distance <- newSTArray b maxBound
  writeArray min_distance src 0
  let aux vertex_queue =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            let edges = adj_list ! u
                f vertex_queue (v, weight) = do
                  let dist_thru_u = dist + weight
                  old_dist <- readArray min_distance v
                  if dist_thru_u >= old_dist then
                    return vertex_queue
                  else do
                    let vertex_queue' = S.delete (old_dist, v) vertex_queue
                    writeArray min_distance v dist_thru_u
                    return $ S.insert (dist_thru_u, v) vertex_queue'
            in
            foldM f vertex_queue' edges >>= aux
  aux (S.singleton (0, src))
  m <- freeze min_distance
  return m
  where b = bounds adj_list
        newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
        newSTArray = newArray
 
readIntList = (Prelude.map (fst . fromJust . B.readInt) . (B.split ' ')) <$> B.getLine

main = do [n, m] <- readIntList 
          edges <- replicateM m $ do
            [src, dst, weight] <- readIntList 
            return (src, (dst, weight))
          print $ dijkstra 1 (graph (1, n) edges) ! n

--adj_list :: Array Char [(Char, Int)]
--adj_list = listArray ('a', 'f') [ [('b',7), ('c',9), ('f',14)],
--                                  [('a',7), ('c',10), ('d',15)],
--                                  [('a',9), ('b',10), ('d',11), ('f',2)],
--                                  [('b',15), ('c',11), ('e',6)],
--                                  [('d',6), ('f',9)],
--                                  [('a',14), ('c',2), ('e',9)] ]
--main :: IO ()
--main = do
--  let (min_distance, previous) = dijkstra 'a' ' ' adj_list
--  putStrLn $ "Distance from a to e: " ++ show (min_distance ! 'e')
--  let path = shortest_path_to 'e' ' ' previous
--  putStrLn $ "Path: " ++ show path
