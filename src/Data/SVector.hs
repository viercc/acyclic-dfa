{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE RankNTypes #-}
module Data.SVector(
  -- * Core types
  SVector(), Key(), KeySpec(),
  getRawVector, fromRawVector, getKey, keyIsInt,

  -- * Other functions
  indices, toAssoc,
  accum,
  imap,
  (!),
  propagate
) where

import Prelude hiding (length)

import qualified Data.Vector as V
import Data.Foldable
import Data.SVector.Internal

import Data.Coerce

getRawVector :: SVector s a -> V.Vector a
getRawVector = coerce

fromRawVector :: V.Vector a -> (forall s. SVector s a -> (Int -> Maybe (Key s)) -> r) -> r
fromRawVector v cont = cont (SV v) check
  where check i | 0 <= i && i < V.length v = Just (Key i)
                | otherwise                = Nothing

indices :: SVector s a -> [Key s]
indices (SV nv) = Key <$> [0 .. V.length nv - 1]

toAssoc :: SVector s a -> [(Key s, a)]
toAssoc v = zip (indices v) (toList v)

accum :: (a -> b -> a) -> SVector s a -> [(Key s, b)] -> SVector s a
accum f = coerce (V.accum f)

imap :: forall s a b. (Key s -> a -> b) -> SVector s a ->  SVector s b
imap = coerce (V.imap @a @b)

-- | Safe indexing
(!) :: forall s a. SVector s a -> Key s -> a
(!) = coerce (V.unsafeIndex @a)

propagate ::
  (Functor f, Functor g) =>
  (f (g (Key s)) -> g (f (Key s))) ->
  SVector s (f (Key s)) ->
  SVector s (g (f (Key s)))
propagate fiddle vec = table
  where
    step fx = fiddle (fmap queryContext fx)
    queryContext x = x <$ (table ! x)
    table = fmap step vec
