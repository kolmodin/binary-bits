{-# LANGUAGE RankNTypes, MagicHash, BangPatterns, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits.Get
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  portable (should run where the package binary runs)

module Data.Binary.Bits.Internal
   ( make_mask
   , mask
   , bit_offset
   , byte_offset
   , reverseBits
   , FastBits(..)
   )
where

import Data.Word
import Data.Bits

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
#endif

-- | make_mask 3 = 00000111
make_mask :: (FastBits a, Num a) => Int -> a
make_mask n = (1 `fastShiftL` fromIntegral n) - 1
{-# SPECIALIZE make_mask :: Int -> Int #-}
{-# SPECIALIZE make_mask :: Int -> Word #-}
{-# SPECIALIZE make_mask :: Int -> Word8 #-}
{-# SPECIALIZE make_mask :: Int -> Word16 #-}
{-# SPECIALIZE make_mask :: Int -> Word32 #-}
{-# SPECIALIZE make_mask :: Int -> Word64 #-}

-- | Keep only the n least-significant bits of the given value
mask :: (FastBits a, Num a) => Int -> a -> a
mask n v = v .&. make_mask n
{-# INLINE mask #-}

-- | Compute bit offset (equivalent to x `mod` 8 but faster)
bit_offset :: Int -> Int
bit_offset n = make_mask 3 .&. n
{-# INLINE bit_offset #-}

-- | Compute byte offset (equivalent to x `div` 8 but faster)
byte_offset :: Int -> Int
byte_offset n = n `shiftR` 3
{-# INLINE byte_offset #-}

-- | Reverse the @n@ least important bits of the given value
reverseBits :: (Num a, FastBits a, Bits a) => Int -> a -> a
reverseBits n value = rec value n 0
   where
      -- rec v i r, where
      --    v is orginal value shifted
      --    i is the remaining number of bits
      --    r is current value
      rec 0 0 r = r
      rec 0 i r = r `fastShiftL` i
      rec v i r = rec (v `fastShiftR` 1) (i-1) ((r `fastShiftL` 1) .|. (v .&. 0x1))


---------------------------------------------------------------------
-- Unchecked shifts, from the "binary" package

-- | Class for types supporting fast bit shifting
class Bits a => FastBits a where
   fastShiftR :: a -> Int -> a
   fastShiftR = shiftR

   fastShiftL :: a -> Int -> a
   fastShiftL = shiftL

   {-# INLINE fastShift #-}
   fastShift :: a -> Int -> a
   fastShift x n
      | n > 0 = fastShiftL x n
      | n < 0 = fastShiftR x (negate n)
      | otherwise = x

instance FastBits Word8 where
   fastShiftR = shiftr_w8
   fastShiftL = shiftl_w8

instance FastBits Word16 where
   fastShiftR = shiftr_w16
   fastShiftL = shiftl_w16

instance FastBits Word32 where
   fastShiftR = shiftr_w32
   fastShiftL = shiftl_w32

instance FastBits Word64 where
   fastShiftR = shiftr_w64
   fastShiftL = shiftl_w64

instance FastBits Int

instance FastBits Word


shiftl_w8  :: Word8  -> Int -> Word8
shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

shiftr_w8  :: Word8  -> Int -> Word8
shiftr_w16 :: Word16 -> Int -> Word16
shiftr_w32 :: Word32 -> Int -> Word32
shiftr_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w8  (W8# w)  (I# i) = W8#  (w `uncheckedShiftL#`   i)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

shiftr_w8  (W8#  w) (I# i) = W8# (w `uncheckedShiftRL#`   i)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`  i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`  i)


#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#`  i)
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
foreign import ccall unsafe "stg_uncheckedShiftRL64"
    uncheckedShiftRL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#`  i)
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)
#endif

#else
shiftl_w8  = shiftL
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL

shiftr_w8 = shiftR
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif
