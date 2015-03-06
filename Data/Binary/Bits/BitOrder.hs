-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits.Get
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
--                (c) Sylvain Henry 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  portable (should run where the package binary runs)

module Data.Binary.Bits.BitOrder
   ( BitOrder(..)
   , BitOrderable(..)
   )
where

-- | Bit order
--
-- E.g. two words of 5 bits: ABCDE, VWXYZ
--    - BB: ABCDEVWX YZxxxxxx
--    - LL: XYZABCDE xxxxxxVW
--    - BL: EDCBAZYX WVxxxxxx
--    - LB: XWVEDCBA xxxxxxZY
data BitOrder
   = BB  -- ^ Big-endian bytes and bits
   | LB  -- ^ Little-endian bytes, big-endian bits
   | BL  -- ^ Big-endian bytes, little-endian bits
   | LL  -- ^ Little-endian bytes and bits
   deriving (Show)


class Monad m => BitOrderable m where
   -- | Set the current bit-order
   setBitOrder :: BitOrder -> m ()

   -- | Retrieve the current bit-order
   getBitOrder    :: m BitOrder

   -- | Perform the given action with the given bit-order
   withBitOrder   :: BitOrder -> m a -> m a
   withBitOrder bo act = do
      bo' <- getBitOrder
      setBitOrder bo
      r <- act
      setBitOrder bo'
      return r
