{-# LANGUAGE BangPatterns #-}
{- | \"Infinite\" Mutable sequence. -}
module Data.MSeq(
    MSeq(),
    new,
    read, write, modify, sliceFrom, writeMany,
    unsafeRead, unsafeWrite, unsafeModify,
    toMVector
) where

import Prelude hiding (read)

import Control.Monad.Primitive
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST.Unsafe (unsafeInterleaveST)

data MSeq s a =
  MSeq {-# UNPACK #-} !(MV.MVector s a) {- Lazy -} (MSeq s a)

type Location s a = (MV.MVector s a, Int)

initialCap :: Int
initialCap = 11

capStep :: Int -> Int
capStep i = min (2 * i + 1) 0xFFFFF -- = 2^20 - 1

new :: (PrimMonad m) => m (MSeq (PrimState m) a)
new = stToPrim $ go initialCap
  where
    go !cap = 
      do v <- MV.new cap
         vs <- unsafeInterleaveST $ go (capStep cap)
         return (MSeq v vs)

unsafeAddr :: MSeq s a -> Int -> Location s a
unsafeAddr = go
  where
    go (MSeq v vs) i
      | i < n     = (v, i)
      | otherwise = go vs (i - n)
      where n = MV.length v

addr :: MSeq s a -> Int -> Location s a
addr ms i
  | 0 <= i    = unsafeAddr ms i
  | otherwise = error $ "Negative index: " ++ show i

readAddr :: (PrimMonad m) => Location (PrimState m) a -> m a
readAddr (v,i) = MV.unsafeRead v i

writeAddr :: (PrimMonad m) => Location (PrimState m) a -> a -> m ()
writeAddr (v,i) = MV.unsafeWrite v i

modifyAddr :: (PrimMonad m) => Location (PrimState m) a -> (a -> a) -> m ()
modifyAddr (v,i) f = MV.unsafeModify v f i

read, unsafeRead :: (PrimMonad m) => MSeq (PrimState m) a -> Int -> m a
read gv i = readAddr (addr gv i)
unsafeRead gv i = readAddr (unsafeAddr gv i)

write, unsafeWrite :: (PrimMonad m) => MSeq (PrimState m) a -> Int -> a -> m ()
write gv i = writeAddr (addr gv i)
unsafeWrite gv i = writeAddr (unsafeAddr gv i)

sliceFrom :: Int -> MSeq s a -> MSeq s a
sliceFrom i
  | i < 0 = error $ "Negative starting index:" ++ show i
  | otherwise = go i
  where
    go k (MSeq v vs)
      | k < n     = MSeq (MV.slice k (n - k) v) vs
      | otherwise = go (k - n) vs
      where n = MV.length v

writeMany :: (PrimMonad m) => MSeq (PrimState m) a -> [a] -> m ()
writeMany = go 0
  where
    go _ _ [] = return ()
    go i vs@(MSeq v vs') (a:as)
      | i < MV.length v = MV.unsafeWrite v i a >> go (i+1) vs as
      | otherwise       = go 0 vs' (a:as)

modify, unsafeModify :: (PrimMonad m) => MSeq (PrimState m) a ->
    (a -> a) -> Int -> m ()
modify gv f i = modifyAddr (addr gv i) f
unsafeModify gv f i = modifyAddr (unsafeAddr gv i) f

toMVector :: (PrimMonad m) =>
  Int -> MSeq (PrimState m) a -> m (MV.MVector (PrimState m) a)
toMVector totalLen ms =
  do mv <- MV.new totalLen
     loop mv 0 ms totalLen
     return mv
  where
    loop _ _ _ 0 = return ()
    loop mv i (MSeq v vs) n =
      do let copyLen = min n (MV.length v)
             dst = MV.slice i copyLen mv
             src = MV.slice 0 copyLen v
         MV.unsafeCopy dst src
         loop mv (i + copyLen) vs (n - copyLen)
