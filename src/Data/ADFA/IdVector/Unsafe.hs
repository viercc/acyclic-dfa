{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
module Data.ADFA.IdVector.Unsafe(
  IdVector(..)
) where

import qualified Data.Vector as V
import Control.DeepSeq

-- | Vector indexed by NodeId
newtype IdVector s a = IV (V.Vector a)
  deriving (Show, Read, Eq, Ord, NFData,
            Functor, Foldable, Traversable)
