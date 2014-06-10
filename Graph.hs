{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}

module Graph (Graph (..), Weight (..), Dense (..), Sparse (..)) where 
import Data.Array
import Data.Monoid

class (Ord a, Num a) => Weight a where
    infinity :: a
    zero :: a

class (Enum v, Ord v, Weight w) => Graph a v w | a -> v, a -> w where
  graph :: (v, v) -> [(v, v, w)] -> a
  adjacent :: a -> v -> [(v, w)]
  weight :: a -> (v, v) -> w 
  vertices :: a -> [v]
  vbounds :: a -> (v, v)

newtype Dense v w = Dense (Array (v, v) w)

instance (Ix v, Enum v, Weight w) => Graph (Dense v w) v w where
  graph (v1, vn) edges = Dense $ accumArray min infinity ((v1, v1), (vn, vn)) edges'
    where edges' = map (\(x, y, z) -> ((x, y), z)) edges
  adjacent (Dense g) v = [(i, g ! (v, i)) | i <- vertices (Dense g)]
  weight (Dense g) p = g ! p
  vertices g = range (vbounds g) 
  vbounds (Dense g) = (v1, vn) where ((v1, _), (vn, _)) = bounds g

newtype Sparse v w = Sparse (Array v [(v, w)])

instance (Ix v, Enum v, Weight w) => Graph (Sparse v w) v w where
  graph (v1, vn) edges = Sparse $ accumArray (flip (:)) [] (v1, vn) edges'
    where edges' = map (\(x, y, z) -> (x, (y, z))) edges
  adjacent (Sparse g) v = g ! v
  weight (Sparse g) (v1, v2) = maybe infinity id $ lookup v2 (g ! v1)
  vertices g = range (vbounds g) 
  vbounds (Sparse g) = bounds g
