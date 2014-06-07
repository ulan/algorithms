import Data.List
import qualified Data.List.Key as K
import Data.Map ((!), fromList, fromListWith, adjust, keys, Map, lookup)
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as B


type Vertex = Int
type Weight = Int
type Distance = Int 
type Edge = (Vertex, (Vertex, Weight))

infinity = 100000000

buildGraph :: Ord a => [(a, a, Weight)] -> Map a [(a, Weight)]
buildGraph g = fromListWith (++) $ g >>=
               \(a,b,d) -> [(a,[(b,d)])]
 
dijkstra :: Ord a => a -> Map a [(a, Weight)] -> Map a Weight
dijkstra source graph =
    f (fromList [(v, if v == source then 0 else infinity) | v <- keys graph]) (keys graph) where
    f ds [] = ds
    f ds q  = f (foldr relax ds $ graph ! m) (delete m q) where
              m = K.minimum (ds !) q
              relax (e,d) = adjust (min (ds ! m + d)) e
 
readIntList = (Prelude.map (fst . fromJust . B.readInt) . (B.split ' ')) <$> B.getLine

main = do [n, m] <- readIntList 
          edges <- replicateM m $ do
            [src, dst, weight] <- readIntList 
            return (src, dst, weight)
          print $ fromJust $ Data.Map.lookup n $ dijkstra 1 (buildGraph edges)


--shortestPath :: Ord a => a -> a -> Map a [(a, Weight)] -> [a]
--shortestPath from to graph = reverse $ f to where
--    f x = x : maybe [] f (snd $ dijkstra from graph ! x)
 
--main :: IO ()
--main = do let g = buildGraph [('a','c',2), ('a','d',6), ('b','a',3),
--                              ('b','d',8), ('c','d',7), ('c','e',5),
--                              ('d','e',10)]
--          print $ shortestPath 'a' 'e' g == "ace"