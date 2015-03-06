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
   )
where

-- | Bit order
--
-- E.g. two words of 5 bits: ABCDE, VWXYZ
--    - BB: ABCDEVWX YZxxxxxx
--    - LB: XYZABCDE xxxxxxVW
--    - BL: EDCBAZYX WVxxxxxx
--    - LL: XWVEDCBA xxxxxxZY
data BitOrder
   = BB  -- ^ Big-endian bytes and bits
   | LB  -- ^ Little-endian bytes, big-endian bits
   | BL  -- ^ Big-endian bytes, little-endian bits
   | LL  -- ^ Little-endian bytes and bits
   deriving (Show)
