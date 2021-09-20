{-# LANGUAGE BangPatterns #-}
{- | \"Infinite\" Mutable sequence. -}
module Data.MSeq(
    MSeq(),
    new,
    read, write, modify,
    toMVector
) where

import Prelude hiding (read)

import Control.Monad.Primitive
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Data.Bits

newtype MSeq s a = MSeq (V.Vector (MV.MVector s a))

type Location = (Int, Int)

-- Int = { i :: Integer | -2^(sizeofInt-1) <= i < 2^(sizeofInt-1) }
sizeofInt :: Int
sizeofInt = finiteBitSize (0 :: Int)

-- MSeq s a = (Vec (2^k) a, Vec (2^k) a, Vec (2^(k+1)) a, ..., Vec (2^(n-2)) a)
--   where k = initialSizeBits
--         n = sizeofInt
--         2^k + 2^k + 2^(k+1) + ... + 2^(n-2) = 2^(n-1)
initialSizeBits, numChunks :: Int
initialSizeBits = 4
numChunks = sizeofInt - initialSizeBits

chunkSizeAt :: Int -> Int
chunkSizeAt i
  | i < 0  = error "negative?"
  | i == 0 = bit initialSizeBits
  | otherwise = bit (initialSizeBits + i - 1) 

new :: (PrimMonad m) => m (MSeq (PrimState m) a)
new = stToPrim $ MSeq <$> V.generateM numChunks gen
  where
    gen i = unsafeInterleaveST $ MV.new (chunkSizeAt i)

addr :: Int -> Location
addr i
  | i < 0                = error $ "Negative Index: " ++ show i
  | i == 0               = (0, 0)
  | k <= initialSizeBits = (0, i)
  | otherwise            = (k - initialSizeBits, r)
  where
    -- 2^(k-1) <= i < 2^k, only used when i /= 0
    k = sizeofInt - countLeadingZeros i
    
    -- r = i - 2^(k-1)
    r = clearBit i (k-1)

readAddr :: (PrimMonad m) => MSeq (PrimState m) a -> Location -> m a
readAddr (MSeq vv) (i,j) = MV.unsafeRead (V.unsafeIndex vv i) j

writeAddr :: (PrimMonad m) => MSeq (PrimState m) a -> Location -> a -> m ()
writeAddr (MSeq vv) (i,j) = MV.unsafeWrite (V.unsafeIndex vv i) j

modifyAddr :: (PrimMonad m) => MSeq (PrimState m) a -> (a -> a) -> Location -> m ()
modifyAddr (MSeq vv) op (i,j) = MV.unsafeModify (V.unsafeIndex vv i) op j

read :: (PrimMonad m) => MSeq (PrimState m) a -> Int -> m a
read gv = readAddr gv . addr

write :: (PrimMonad m) => MSeq (PrimState m) a -> Int -> a -> m ()
write gv = writeAddr gv . addr

modify :: (PrimMonad m) => MSeq (PrimState m) a ->
    (a -> a) -> Int -> m ()
modify gv f = modifyAddr gv f . addr

toMVector :: (PrimMonad m) =>
  Int -> MSeq (PrimState m) a -> m (MV.MVector (PrimState m) a)
toMVector totalLen (MSeq vv) =
  do mv <- MV.new totalLen
     loop mv 0 (V.toList vv) totalLen
     return mv
  where
    loop _ _ _  0 = return ()
    loop _ _i [] _n = error $ "Unreachable!" ++ show (_i, _n)
    loop mv i (v:vs) n =
      do let copyLen = min n (MV.length v)
             dst = MV.slice i copyLen mv
             src = MV.slice 0 copyLen v
         MV.copy dst src
         loop mv (i + copyLen) vs (n - copyLen)
