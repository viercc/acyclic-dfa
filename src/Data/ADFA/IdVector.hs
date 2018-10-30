{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.ADFA.IdVector(
  IdVector(),
  length, toList,
  indices, toAssoc,
  foldl',
  accum,
  imap,
  (!),
  propagate
) where

import Prelude hiding (length)

import Data.ADFA.NodeId(NodeId(..))

import qualified Data.Vector as V
import Data.ADFA.IdVector.Unsafe

import Data.Coerce

length :: IdVector s a -> Int
length (IV nv) = V.length nv

toList :: forall s a. IdVector s a -> [a]
toList = coerce (V.toList @a)

indices :: IdVector s a -> [NodeId s]
indices (IV nv) = NodeId <$> [0 .. V.length nv - 1]

toAssoc :: IdVector s a -> [(NodeId s, a)]
toAssoc (IV nv) = zip (NodeId <$> [0..]) (V.toList nv)

accum :: forall a b s. (a -> b -> a) -> IdVector s a -> [(NodeId s, b)] -> IdVector s a
accum f = coerce (V.accum f)

foldl' :: forall a b s. (a -> b -> a) -> a -> IdVector s b -> a
foldl' = coerce (V.foldl' @a @b)

imap :: forall s a b. (NodeId s -> a -> b) -> IdVector s a ->  IdVector s b
imap = coerce (V.imap @a @b)

-- | Safe indexing
(!) :: forall s a. IdVector s a -> NodeId s -> a
(!) = coerce (V.unsafeIndex @a)

propagate ::
  (Functor f, Functor g) =>
  (f (g (NodeId s)) -> g (f (NodeId s))) ->
  IdVector s (f (NodeId s)) ->
  IdVector s (g (f (NodeId s)))
propagate fiddle vec = table
  where
    step fx = fiddle (fmap queryContext fx)
    queryContext x = x <$ (table ! x)
    table = fmap step vec
