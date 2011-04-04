{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -funbox-strict-fields #-}
module Data.Graph.Isomorphism.Graph where

import Data.Vector.Unboxed
import Data.Functor
import Prelude hiding (replicate, map, length, (++))

data Graph = Graph !Int !(Vector Bool)

type Node = Int

mkGraph :: Int -> [(Node, Node)] -> Graph
mkGraph n edges = mkGraph' n (fromList edges)

mkGraph' :: Int -> Vector (Node, Node) -> Graph
mkGraph' n !edges = Graph n g1
  where	g0 = replicate (n * n) False
	m = length edges
	ixs1 = map (\ (i, j) -> n * i + j) edges
	ixs2 = map (\ (i, j) -> n * j + i) edges
	g1 = unsafeUpdate g0 (map (\ !i -> (i, True)) (ixs1 ++ ixs2))