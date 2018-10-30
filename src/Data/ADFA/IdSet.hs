{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.ADFA.IdSet(
  IdSet(),
  empty, singleton,
  fromList, fromAscList, toAscList,
  member, notMember,
  insert, delete,
  union, difference, intersection,
  foldl',
  toList
) where

import Data.ADFA.NodeId(NodeId(..))

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Coerce

-- | Set of @(NodeId s)@.
newtype IdSet s = IdSet IntSet
  deriving (Show, Read, Eq, Ord)

empty :: IdSet s
empty = coerce IntSet.empty

singleton :: NodeId s -> IdSet s
singleton = coerce IntSet.singleton

fromList :: [NodeId s] -> IdSet s
fromList = coerce IntSet.fromList

fromAscList :: [NodeId s] -> IdSet s
fromAscList = coerce IntSet.fromAscList

toAscList :: IdSet s -> [NodeId s]
toAscList = coerce IntSet.toAscList

member, notMember :: NodeId s -> IdSet s -> Bool
member    = coerce IntSet.member
notMember = coerce IntSet.notMember

insert, delete :: NodeId s -> IdSet s -> IdSet s
insert = coerce IntSet.insert
delete = coerce IntSet.delete

union, difference, intersection :: IdSet s -> IdSet s -> IdSet s
union        = coerce IntSet.union
difference   = coerce IntSet.difference
intersection = coerce IntSet.intersection

foldl' :: forall a s. (a -> NodeId s -> a) -> a -> IdSet s -> a
foldl' = coerce (IntSet.foldl' @a)

toList :: IdSet s -> [NodeId s]
toList = coerce IntSet.toList
