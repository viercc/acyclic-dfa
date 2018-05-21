{-| Acyclic Deterministic Finite State Automaton for finite language -}
module Data.ADFA(
  ADFA(),
  Node(),
  toTable,
  fromTable,
  nodeList,
  edgeList,
  -- * Construction
  empty, string, strings,
  -- * Query
  isEmpty,
  enumerate, stringCount,
  match,
  -- * Combines
  union, unions, intersection, difference, append,
  -- * Remove redundancy
  minify, topSort,
  -- * Debug
  debugShow, debugPrint
) where

import           Data.List           (foldl')

import           Data.ADFA.Algorithm
import           Data.ADFA.Internal

import qualified Data.Vector     as V
import qualified Data.Map.Strict as Map

-- | Take union of all ADFAs in a list.
unions :: (Ord c) => [ADFA c] -> ADFA c
unions = foldl' union empty

nodeList :: ADFA c -> [(Int, Bool)]
nodeList (MkDFA nodes _) =
  zipWith (\i n -> (i, isAccepted n)) [0..] (V.toList nodes)

edgeList :: ADFA c -> [(Int, c, Int)]
edgeList (MkDFA nodes _) = V.ifoldr f [] nodes
  where
    f i n r = [ (i, c, y) | (c,NodeId y) <- Map.toList (outEdges n) ] ++ r
