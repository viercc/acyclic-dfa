{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.ADFA.IdMap(
  IdMap(),
  size,
  empty, singleton,
  fromList, fromAscList, toAscList,
  lookup, insert
) where

import Prelude hiding (lookup)

import Data.ADFA.NodeId(NodeId(..))

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Coerce

-- | Map keyed by @(NodeId s)@.
newtype IdMap s a = IdMap (IntMap a)
  deriving (Show, Read, Eq, Ord,
            Functor, Foldable, Traversable)

size :: forall s a. IdMap s a -> Int
size = coerce (IntMap.size @a)

empty :: forall s a. IdMap s a
empty = coerce (IntMap.empty @a)

singleton :: forall s a. NodeId s -> a -> IdMap s a
singleton = coerce (IntMap.singleton @a)

fromList :: forall s a. [(NodeId s, a)] -> IdMap s a
fromList = coerce (IntMap.fromList @a)

fromAscList :: forall s a. [(NodeId s, a)] -> IdMap s a
fromAscList = coerce (IntMap.fromAscList @a)

toAscList :: forall s a. IdMap s a -> [(NodeId s, a)]
toAscList = coerce (IntMap.toAscList @a)

lookup :: forall s a. NodeId s -> IdMap s a -> Maybe a
lookup = coerce (IntMap.lookup @a)

insert :: forall s a. NodeId s -> a -> IdMap s a -> IdMap s a
insert = coerce (IntMap.insert @a)
