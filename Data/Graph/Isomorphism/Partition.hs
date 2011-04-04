{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts #-}
module Data.Graph.Isomorphism.Partition where

import qualified Data.Map as M
import qualified Data.Vector as V

class Ord i => Partition p i | p -> i where
  toList :: p a -> [(i, [a])]
  fromList :: [(i, a)] -> p a
  toIndexed :: p a -> Indexed a
  toIndexed p = Indexed (V.fromList [part | (i, (_, part)) <- zip [0..] (toList p)])

newtype MapPart i a = MapPart (M.Map i [a])

instance Ord i => Partition (MapPart i) i where
  fromList xs = MapPart (M.fromListWith (++) [(i, [a]) | (i, a) <- xs])
  toList (MapPart m) = M.assocs m
  toIndexed (MapPart m) = Indexed (V.fromListN (M.size m) (M.elems m))

newtype Indexed a = Indexed (V.Vector [a])

instance Partition Indexed Int where
  toList (Indexed xs) = V.toList (V.zip (V.enumFromN 0 (V.length xs)) xs)
  fromList xs = Indexed (V.accum (flip (:)) (V.replicate n []) xs) where
    n = maximum (map fst xs) + 1
  toIndexed = id

partBy :: (Partition p i, Partition p' (i, i')) =>
  (i -> a -> i') -> p a -> p' a
partBy f part = fromList [((i, f i x), x) | (i, xs) <- toList part, x <- xs]