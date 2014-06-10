module Util (readGraph) where 

import Control.Monad
import Control.Applicative
import qualified Data.ByteString.Char8 as B

readIntList = (map (fst . fromJust . B.readInt) . (B.split ' ')) <$> B.getLine

readGraph = do [n, m] <- readIntList
               edges <- replicateM m $ do
                 [src, dst, weight] <- readIntList 
                 return (src, dst, weight)
               return (n, m, edges) 
