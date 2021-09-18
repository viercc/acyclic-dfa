{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.SVector.Internal(
  SVector(..), Key(..), KeySpec(..), getKey, keyIsInt
) where

import qualified Data.Vector as V
import Control.DeepSeq(NFData)
import Data.Reflection
import Data.Proxy
import Data.Type.Coercion.Sub

-- | Vector with type-safe indexing
newtype SVector s a = SV (V.Vector a)
  deriving newtype (Show, Read, Eq, Ord, NFData, Functor, Foldable)
  deriving stock Traversable
type role SVector nominal representational

newtype Key s = Key Int
  deriving newtype (Show, Eq, Ord, NFData)
type role Key nominal

getKey :: Key s -> Int
getKey = upcastWith keyIsInt

keyIsInt :: Sub (Key s) Int
keyIsInt = sub

newtype KeySpec s = KeySpec { _kslength :: Int }
type role KeySpec nominal

instance Reifies s (KeySpec s) => Bounded (Key s) where
  minBound = Key 0
  maxBound = Key (_kslength (reflect (Proxy @s)) - 1)

instance Reifies s (KeySpec s) => Enum (Key s) where
  toEnum i = if minBound <= k && k <= maxBound then k else err
    where k = Key i :: Key s
          err = error $ "Out of bound:" ++ show i
  fromEnum (Key i) = i

